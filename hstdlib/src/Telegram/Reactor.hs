{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Telegram.Reactor where

import qualified Data.Telegram.Authorization as Authorization
import Data.Telegram.Parameters (Parameters, tdlibParams, tdlibProxy)
import qualified Data.Telegram.Parameters as P
import Data.Telegram.User (User(User))
import qualified Data.Telegram.User as User
import qualified Telegram.TDLib.API as TDLib
import qualified Telegram.TDLib.Bindings as TDLib

import Control.Arrow ((&&&))
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
  ( TVar
  , newTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Exception
  ( Exception
  , bracket
  , finally
  , throwIO
  , uninterruptibleMask_
  )
import Control.Lens (to)
import Control.Lens.At (at, ix)
import Control.Lens.Fold (has, preview)
import Control.Lens.Getter (Getting)
import Control.Lens.Operators ((<<.~), (?~), (^.), (^?))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Generics.Product.Fields (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid (First)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Word (Word64)

data Reactor = Reactor
  { eventLoopThread :: Async ()
  , updatesThread :: Async ()
  , subscriptions :: IORef (HashMap Word64 (MVar TDLib.Object))
  , nextSubscriptionId :: IORef Word64
  , state :: State
  , client :: TDLib.TDLibClient
  , params :: Parameters
  }

data State = State
  { users :: TVar (IntMap (TVar User))
  , authorizationState :: TVar (Maybe Authorization.State)
  }

-- TODO: handle errors during startup
launchReactor :: MonadIO m => Parameters -> m Reactor
launchReactor parameters =
  liftIO $ uninterruptibleMask_ $
  mdo updatesChan <- newChan
      subs <- newIORef HM.empty
      subsId <- newIORef 0
      st <- mkState
      eloop <-
        asyncWithUnmask $
        eventLoop reactor (P.receptionTimeout parameters) updatesChan
      uloop <-
        asyncWithUnmask $ \unmask -> unmask $ handleUpdates reactor updatesChan
      cl <- TDLib.createClient
      let reactor =
            Reactor
              { eventLoopThread = eloop
              , updatesThread = uloop
              , subscriptions = subs
              , nextSubscriptionId = subsId
              , state = st
              , client = cl
              , params = parameters
              }
      pure reactor
  where
    mkState :: IO State
    mkState = do
      usrs <- newTVarIO IM.empty
      astate <- newTVarIO Nothing
      pure $ State {users = usrs, authorizationState = astate}

killReactor :: MonadIO m => Reactor -> m ()
killReactor = liftIO . cancel . eventLoopThread

withReactor :: MonadIO m => Parameters -> (Reactor -> IO a) -> m a
withReactor p = liftIO . bracket (launchReactor p) killReactor

eventLoop :: Reactor -> Double -> Chan TDLib.Update -> (IO () -> IO ()) -> IO ()
eventLoop reactor timeout updatesChan unmask =
  unmask loop `finally` do
    TDLib.destroyClient (client reactor)
    writeChan updatesChan (TDLib.UpdateAuthorizationState TDLib.Closed)
  where
    loop :: IO ()
    loop = do
      msg <- TDLib.recv (client reactor) timeout
      case msg of
        Just (TDLib.WithExtra obj mExchangeId) -> do
          for_ mExchangeId (handleResponse obj)
          for_ (obj ^? TDLib._UpdateObj) (writeChan updatesChan)
          unless
            (has
               (TDLib._UpdateObj . TDLib._UpdateAuthorizationState .
                TDLib._Closed)
               obj)
            loop
        Nothing -> pure ()
    handleResponse :: TDLib.Object -> TDLib.LongNumber Word64 -> IO ()
    handleResponse obj (TDLib.LongNumber exchangeId) = do
      mMailbox <-
        atomicModifyIORef'
          (subscriptions reactor)
          (swap . (at exchangeId <<.~ Nothing))
      for_ mMailbox $ \mailbox -> putMVar mailbox obj

handleUpdates :: Reactor -> Chan TDLib.Update -> IO ()
handleUpdates reactor incoming = loop
  where
    loop :: IO ()
    loop = do
      update <- readChan incoming
      for_ (update ^? TDLib._UpdateUser) handleUserUpdate
      for_
        (update ^? TDLib._UpdateAuthorizationState)
        handleAuthorizationStateUpdate
      unless (has (TDLib._UpdateAuthorizationState . TDLib._Closed) update) loop
    handleAuthorizationStateUpdate :: TDLib.AuthorizationState -> IO ()
    handleAuthorizationStateUpdate astate = do
      let translated = Authorization.translate astate
      atomically $
        writeTVar (authorizationState . state $ reactor) (Just translated)
      case translated of
        Authorization.WaitParameters ->
          TDLib.send
            (client reactor)
            (TDLib.WithExtra
               (TDLib.SetTdlibParameters (tdlibParams (params reactor)))
               (Nothing :: Maybe ()))
        Authorization.WaitEncryptionKey _encState -> do
          TDLib.send
            (client reactor)
            (TDLib.WithExtra
               (TDLib.CheckDatabaseEncryptionKey
                  (P.encryptionKey . P.storage . params $ reactor))
               (Nothing :: Maybe ()))
          for_ (tdlibProxy $ params reactor) $ \proxy ->
            TDLib.send (client reactor) $
            TDLib.WithExtra (TDLib.SetProxy proxy) (Nothing :: Maybe ())
        Authorization.WaitPhoneNumber ->
          TDLib.send
            (client reactor)
            (TDLib.WithExtra
               (TDLib.SetAuthenticationPhoneNumber
                  (P.phoneNumber $ params reactor)
                  False
                  False)
               (Nothing :: Maybe ()))
        _ -> pure ()
    handleUserUpdate :: TDLib.User -> IO ()
    handleUserUpdate user =
      atomically $ do
        userMap <- readTVar (users . state $ reactor)
        let uid = fromIntegral $ user ^. field @"id"
        case userMap ^? ix uid of
          Just tuser -> writeTVar tuser translateUser
          Nothing -> do
            tuser <- newTVar translateUser
            writeTVar (users . state $ reactor) (userMap & at uid ?~ tuser)
      where
        translateUser :: User
        translateUser =
          User
            { User.firstName = user ^. field @"firstName"
            , User.lastName = user ^. field @"lastName"
            , User.username = user ^. field @"username"
            , User.phoneNumber = user ^. field @"phoneNumber"
            , User.status =
                case user ^. field @"status" of
                  TDLib.UserStatusEmpty -> User.Empty
                  TDLib.UserStatusLastMonth -> User.LastMonth
                  TDLib.UserStatusLastWeek -> User.LastWeek
                  TDLib.UserStatusOffline wasOnline ->
                    User.Offline
                      (posixSecondsToUTCTime $ fromIntegral wasOnline)
                  TDLib.UserStatusOnline expires ->
                    User.Online (posixSecondsToUTCTime $ fromIntegral expires)
                  TDLib.UserStatusRecently -> User.Recently
            , User.verificationStatus =
                if user ^. field @"isVerified"
                  then User.Verified
                  else User.Unverified
            , User.restrictionReason = Nothing -- TODO
            , User.access =
                if user ^. field @"haveAccess"
                  then User.Accessible
                  else User.Inaccesible
            , User.kind =
                case user ^. field @"_type" of
                  TDLib.UserTypeUnknown -> User.Unknown
                  TDLib.UserTypeRegular -> User.Regular
                  TDLib.UserTypeDeleted -> User.Deleted
                  TDLib.UserTypeBot canJoinGroups canReadAllGroupMessages isInline inlineQueryPlaceholder needLocation ->
                    User.Bot
                      (User.BotInfo
                         canJoinGroups
                         canReadAllGroupMessages
                         isInline
                         inlineQueryPlaceholder
                         needLocation)
            , User.languageCode =
                if Text.null (user ^. field @"languageCode")
                  then Nothing
                  else Just (user ^. field @"languageCode")
            }

waitForPassingAuthState :: MonadIO m => Reactor -> (Authorization.State -> Maybe a) -> m a
waitForPassingAuthState reactor getter =
  liftIO $ atomically $ do
    astate <- readTVar . authorizationState . state $ reactor
    case astate >>= getter of
      Just a -> pure a
      Nothing -> retry

waitForAuthState ::
     MonadIO m => Reactor -> Getting (First a) Authorization.State a -> m a
waitForAuthState reactor prism = waitForPassingAuthState reactor (preview prism)

data UnexpectedResponse = UnexpectedResponse
  { request :: TDLib.Function
  , response :: TDLib.Object
  } deriving (Eq, Show)

instance Exception UnexpectedResponse

data TDLibError = TDLibError
  { errorCode :: Int
  , errorText :: Text
  } deriving (Eq, Show)

instance Exception TDLibError

makeRequest ::
     MonadIO m
  => Reactor
  -> Getting (First a) TDLib.Object a
  -> TDLib.Function
  -> m (Either TDLibError a)
makeRequest reactor getter function =
  liftIO $ do
    subId <- atomicModifyIORef' (nextSubscriptionId reactor) (succ &&& id)
    responseHole <- newEmptyMVar
    atomicModifyIORef'
      (subscriptions reactor)
      (\subs -> (HM.insert subId responseHole subs, ()))
    TDLib.send
      (client reactor)
      (TDLib.WithExtra function (Just $ TDLib.LongNumber subId))
    resp <- takeMVar responseHole
    case resp of
      TDLib.ErrorObj (TDLib.Error code text) ->
        pure $ Left (TDLibError (fromIntegral code) text)
      _ ->
        case resp ^? getter of
          Nothing -> throwIO $ UnexpectedResponse function resp
          Just result -> pure $ Right result

unwrap :: (Exception e, MonadIO m) => Either e a -> m a
unwrap = liftIO . either throwIO pure

newtype UserId = UserId
  { getUserId :: Int32
  } deriving (Eq, Show)

searchContacts ::
     MonadIO m
  => Reactor
  -> Text
  -> Int
  -> m (Either TDLibError (Vector UserId))
searchContacts reactor query limit =
  makeRequest
    reactor
    (TDLib._UsersObj . field @"userIds" . to (fmap UserId))
    (TDLib.SearchContacts query (fromIntegral limit))

searchContacts' :: MonadIO m => Reactor -> Text -> Int -> m (Vector UserId)
searchContacts' reactor query limit =
  unwrap =<< searchContacts reactor query limit

checkAuthCode ::
     MonadIO m
  => Reactor
  -> Maybe (Text, Text)
  -> Text
  -> m (Either TDLibError ())
checkAuthCode reactor name code =
  makeRequest
    reactor
    (TDLib._OkObj . TDLib._Ok)
    (TDLib.CheckAuthenticationCode code (fmap fst name) (fmap snd name))

checkAuthCode' :: MonadIO m => Reactor -> Maybe (Text, Text) -> Text -> m ()
checkAuthCode' reactor name code = unwrap =<< checkAuthCode reactor name code

lookupUsers :: (MonadIO m, Traversable t) => Reactor -> t UserId -> m (t User)
lookupUsers reactor userIds =
  liftIO $ atomically $ do
    userMap <- readTVar (users $ state reactor)
    for userIds $ \uid ->
      case IM.lookup (fromIntegral $ getUserId uid) userMap of
        Nothing -> retry
        Just u -> readTVar u
