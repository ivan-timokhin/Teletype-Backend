{-# LANGUAGE TemplateHaskell #-}

module Telegram.TDLib.API
  ( TDLibClient
  , withClient
  , Parameters(..)
  , AuthorizationState(..)
  , Update(..)
  , _AuthorizationState
  , Object(..)
  , _Update
  , recv
  , Function(..)
  , send
  , createClient
  , destroyClient
  ) where

import Telegram.TDLib.API.AesonOptions
  ( aesonOptions
  , functionOptions
  , objectAesonOptions
  )
import Telegram.TDLib.Bindings
  ( TDLibClient
  , createClient
  , destroyClient
  , recvJSON
  , sendJSON
  )

import Data.Aeson (eitherDecodeStrict', encode)
import Data.Aeson.TH (deriveJSON)
import Control.Exception (bracket, Exception, throwIO)
import Data.Int (Int32)
import Data.ByteString.Lazy (toStrict)
import Control.Lens.TH (makePrisms)
import Data.Traversable (for)
import Data.ByteString (ByteString)

withClient :: (TDLibClient -> IO a) -> IO a
withClient = bracket createClient destroyClient

-- | Contains parameters for TDLib initialisation.
data Parameters = Parameters
  { useTestDC :: Bool -- ^ If set to true, the Telegram test
                      -- environment will be used instead of the
                      -- production environment.
  , databaseDirectory :: String -- ^ The path to the directory for
                                -- the persistent database; if empty,
                                -- the current working directory will
                                -- be used.
  , filesDirectory :: String -- ^ The path to the directory for
                             -- storing files; if empty,
                             -- database_directory will be used.
  , useFileDatabase :: Bool -- ^ If set to true, information about
                            -- downloaded and uploaded files will be
                            -- saved between application restarts.
  , useChatInfoDatabase :: Bool -- ^ If set to true, the library will
                                -- maintain a cache of users, basic
                                -- groups, supergroups, channels and
                                -- secret chats. Implies
                                -- use_file_database.
  , useMessageDatabase :: Bool -- ^ If set to true, the library will
                               -- maintain a cache of chats and
                               -- messages. Implies
                               -- useChatInfoDatabase.
  , useSecretChats :: Bool -- ^ If set to true, support for secret
                           -- chats will be enabled.
  , apiID :: Int32 -- ^ Application identifier for Telegram API
                   -- access, which can be obtained at
                   -- https://my.telegram.org.
  , apiHash :: String -- ^ Application identifier hash for Telegram
                      -- API access, which can be obtained at
                      -- https://my.telegram.org.
  , systemLanguageCode :: String -- ^ IETF language tag of the user's
                                 -- operating system language; must
                                 -- be non-empty.
  , deviceModel :: String -- ^ Model of the device the application is
                          -- being run on; must be non-empty.
  , systemVersion :: String -- ^ Version of the operating system the
                            -- application is being run on; must be
                            -- non-empty.
  , applicationVersion :: String -- ^ Application version; must be
                                 -- non-empty.
  , enableStorageOptimizer :: Bool -- ^ If set to true, old files
                                   -- will automatically be deleted.
  , ignoreFileNames :: Bool -- ^ If set to true, original file names
                            -- will be ignored. Otherwise, downloaded
                            -- files will be saved under names as
                            -- close as possible to the original
                            -- name.
  } deriving (Eq, Show)

deriveJSON (aesonOptions "tdlib") ''Parameters

data AuthorizationState =
  WaitTdlibParameters
  deriving (Eq, Show)

deriveJSON (aesonOptions "authorizationState") ''AuthorizationState

-- A better idea may be to go Object route and have separate type for
-- each update alternative
data Update = AuthorizationState
  { authorizationState :: AuthorizationState
  } deriving (Eq, Show)

deriveJSON (aesonOptions "update") ''Update
makePrisms ''Update

data Object
  = Update Update
  | TDLibParameters Parameters
  deriving (Eq, Show)

deriveJSON objectAesonOptions ''Object
makePrisms ''Object

data ResponseDecodingError = ResponseDecodingError
  { originalResponse :: ByteString
  , errorMessage :: String
  } deriving (Eq, Show)

instance Exception ResponseDecodingError

recv :: TDLibClient -> Double -> IO (Maybe Object)
recv client timeout = do
  responseJSON <- recvJSON client timeout
  for responseJSON $ \r ->
    case eitherDecodeStrict' r of
      Left err ->
        throwIO $
        ResponseDecodingError {originalResponse = r, errorMessage = err}
      Right object -> pure object

data Function = SetTdlibParameters
  { parameters :: Parameters
  } deriving (Eq, Show)

deriveJSON functionOptions ''Function

send :: TDLibClient -> Function -> IO ()
send client request = sendJSON client (toStrict $ encode request)
