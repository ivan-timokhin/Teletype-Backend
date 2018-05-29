{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.TDLib.API
  ( TDLibClient
  , withClient
  , WithExtra(..)
  , Parameters(..)
  , AuthorizationState(..)
  , Update(..)
  , _AuthorizationState
  , Object(..)
  , _UpdateObj
  , recv
  , Function(..)
  , send
  , createClient
  , destroyClient
  ) where

import qualified Telegram.TDLib.API.AesonOptions as AOpt
import Telegram.TDLib.Bindings
  ( TDLibClient
  , createClient
  , destroyClient
  , recvJSON
  , sendJSON
  )

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , (.:?)
  , eitherDecodeStrict'
  , encode
  )
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)
import Control.Exception (bracket, Exception, throwIO)
import Data.Int (Int32)
import Data.ByteString.Lazy (toStrict)
import Control.Lens.TH (makePrisms)
import Data.Traversable (for)
import Data.ByteString (ByteString)
import Control.Monad (join)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

withClient :: (TDLibClient -> IO a) -> IO a
withClient = bracket createClient destroyClient

data WithExtra a b = WithExtra
  { payload :: a
  , extra :: Maybe b
  } deriving (Show, Eq, Functor, Foldable, Traversable)

instance (FromJSON a, FromJSON b) => FromJSON (WithExtra a b) where
  parseJSON =
    A.withObject "WithExtra" $ \v ->
      WithExtra <$> parseJSON (A.Object v) <*> (join <$> v .:? "@extra")

instance (ToJSON a, ToJSON b) => ToJSON (WithExtra a b) where
  toJSON (WithExtra p e) =
    case toJSON p of
      A.Object o -> A.Object (HM.insert "@extra" (toJSON e) o)
      _ ->
        error "Whoops! WithExtra expects Object-like type as a first argument"

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

deriveJSON AOpt.namedCtors ''Parameters

data AuthorizationState
  = WaitTdlibParameters
  | WaitEncryptionKey { isEncrypted :: Bool }
  | WaitPhoneNumber
  deriving (Eq, Show)

deriveJSON (AOpt.prefixedCtors "authorizationState") ''AuthorizationState

data ConnectionState
  = Connecting
  | ConnectingToProxy
  | Ready
  | Updating
  | WaitingForNetwork
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

deriveJSON (AOpt.prefixedCtors "connectionState") ''ConnectionState

data OptionValue
  = OptionValueBoolean { _boolValue :: Bool }
  | OptionValueEmpty
  | OptionValueInteger { _intValue :: Int32 }
  | OptionValueString { _textValue :: Text }
  deriving (Eq, Show)

deriveJSON
  (AOpt.namedCtors
     { A.fieldLabelModifier =
         A.fieldLabelModifier AOpt.namedCtors . dropWhile (/= 'V')
     })
  ''OptionValue

-- A better idea may be to go Object route and have separate type for
-- each update alternative
data Update
  = AuthorizationState { authorizationState :: AuthorizationState }
  | Option { name :: Text
           , value :: OptionValue }
  | ConnectionState { state :: ConnectionState }
  deriving (Eq, Show)

deriveJSON (AOpt.prefixedCtors "update") ''Update
makePrisms ''Update

data Ok =
  Ok
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

deriveJSON AOpt.namedCtors ''Ok

data Object
  = UpdateObj Update
  | ParametersObj Parameters
  | OkObj Ok
  deriving (Eq, Show)

deriveJSON AOpt.anonymousCtors ''Object
makePrisms ''Object

data ResponseDecodingError = ResponseDecodingError
  { originalResponse :: ByteString
  , errorMessage :: String
  } deriving (Eq, Show)

instance Exception ResponseDecodingError

type Response = WithExtra Object

recv :: FromJSON a => TDLibClient -> Double -> IO (Maybe (Response a))
recv client timeout = do
  responseJSON <- recvJSON client timeout
  for responseJSON $ \r ->
    case eitherDecodeStrict' r of
      Left err ->
        throwIO $
        ResponseDecodingError {originalResponse = r, errorMessage = err}
      Right object -> pure object

data Function
  = SetTdlibParameters { parameters :: Parameters }
  | CheckDatabaseEncryptionKey { encryptionKey :: String }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Function

type Request = WithExtra Function

send :: ToJSON a => TDLibClient -> Request a -> IO ()
send client request = sendJSON client (toStrict $ encode request)
