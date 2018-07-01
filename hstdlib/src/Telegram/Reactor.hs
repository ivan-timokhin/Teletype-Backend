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

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
  ( TVar
  , newTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Lens.At (at, ix)
import Control.Lens.Fold (has)
import Control.Lens.Operators ((<<.~), (?~), (^.), (^?))
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Generics.Product.Fields (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Tuple (swap)
import Data.Word (Word64)

data Reactor = Reactor
  { eventLoopThread :: Async ()
  , updatesThread :: Async ()
  , subscriptions :: IORef (HashMap Word64 (MVar TDLib.Object))
  , state :: State
  , client :: TDLib.TDLibClient
  , params :: Parameters
  }

data State = State
  { users :: TVar (IntMap (TVar User))
  , authorizationState :: TVar (Maybe Authorization.State)
  }

-- TODO: handle errors during startup
launchReactor :: Parameters -> IO Reactor
launchReactor parameters =
  mdo updatesChan <- newChan
      subs <- newIORef HM.empty
      st <- mkState
      eloop <-
        async $ eventLoop reactor (P.receptionTimeout parameters) updatesChan
      uloop <- async $ handleUpdates reactor updatesChan
      cl <- TDLib.createClient
      let reactor =
            Reactor
              { eventLoopThread = eloop
              , updatesThread = uloop
              , subscriptions = subs
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

eventLoop :: Reactor -> Double -> Chan TDLib.Update -> IO ()
eventLoop reactor timeout updatesChan = loop
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
               (TDLib._UpdateObj .
                TDLib._UpdateAuthorizationState . TDLib._Closed)
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
        Authorization.WaitParameters -> do
          TDLib.send
            (client reactor)
            (TDLib.WithExtra
               (TDLib.SetTdlibParameters (tdlibParams (params reactor)))
               (Nothing :: Maybe ()))
          for_ (tdlibProxy $ params reactor) $ \proxy ->
            TDLib.send (client reactor) $
            TDLib.WithExtra (TDLib.SetProxy proxy) (Nothing :: Maybe ())
        Authorization.WaitEncryptionKey _encState ->
          TDLib.send
            (client reactor)
            (TDLib.WithExtra
               (TDLib.CheckDatabaseEncryptionKey
                  (P.encryptionKey . P.storage . params $ reactor))
               (Nothing :: Maybe ()))
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
