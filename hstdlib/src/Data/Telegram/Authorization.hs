{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Telegram.Authorization where

import qualified Telegram.TDLib.API as TDLib

import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)
import Data.Data (Data)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)

data EncryptionState
  = Encrypted
  | Unencrypted
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, Data)

data RegistrationState
  = Registered
  | NotRegistered
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, Data)

data CodeType
  = Call Int
  | FlashCall Text
  | Sms Int
  | TelegramMessage Int
  deriving (Eq, Show, Generic, Data)

data CodeInfo = CodeInfo
  { phoneNumber :: Text
  , kind :: CodeType
  , nextCode :: Maybe CodeType
  , timeout :: UTCTime
  } deriving (Eq, Show, Generic, Data)

data PasswordInfo = PasswordInfo
  { passwordHint :: Text
  , hasRecoveryEmailAddress :: Bool
  , recoveryEmailAddressPattern :: Text
  } deriving (Eq, Show, Generic, Data)

data State
  = WaitParameters
  | WaitEncryptionKey EncryptionState
  | WaitCode RegistrationState
             CodeInfo
  | WaitPassword PasswordInfo
  | WaitPhoneNumber
  | Closed
  | Closing
  | LoggingOut
  | Ready
  deriving (Eq, Show, Generic, Data)

translate :: TDLib.AuthorizationState -> State
translate TDLib.WaitTdlibParameters = WaitParameters
translate (TDLib.WaitEncryptionKey isEncrypted) =
  WaitEncryptionKey
    (if isEncrypted
       then Encrypted
       else Unencrypted)
translate (TDLib.WaitCode isRegistered (TDLib.AuthenticationCodeInfo phone kind_ next timeout_)) =
  WaitCode
    (if isRegistered
       then Registered
       else NotRegistered)
    (CodeInfo
       phone
       (translateCodeType kind_)
       (fmap translateCodeType next)
       (posixSecondsToUTCTime $ fromIntegral timeout_))
  where
    translateCodeType :: TDLib.AuthenticationCodeType -> CodeType
    translateCodeType (TDLib.AuthenticationCodeTypeCall len) =
      Call (fromIntegral len)
    translateCodeType (TDLib.AuthenticationCodeTypeFlashCall pat) =
      FlashCall pat
    translateCodeType (TDLib.AuthenticationCodeTypeSms len) =
      Sms (fromIntegral len)
    translateCodeType (TDLib.AuthenticationCodeTypeTelegramMessage len) =
      TelegramMessage (fromIntegral len)
translate (TDLib.WaitPassword hint hasRecovery recoveryPat) =
  WaitPassword $ PasswordInfo hint hasRecovery recoveryPat
translate TDLib.WaitPhoneNumber = WaitPhoneNumber
translate TDLib.Closed = Closed
translate TDLib.Closing = Closing
translate TDLib.LoggingOut = LoggingOut
translate TDLib.Ready = Ready

concat <$>
  traverse
    (deriveJSON A.defaultOptions)
    [ ''EncryptionState
    , ''RegistrationState
    , ''CodeInfo
    , ''CodeType
    , ''PasswordInfo
    , ''State
    ]
