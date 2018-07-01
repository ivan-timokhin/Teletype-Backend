{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Telegram.User
  ( Status(..)
  , BotInfo(..)
  , Kind(..)
  , Verification(..)
  , Access(..)
  , RestrictionReason(..)
  , User(..)
  ) where

import Data.Data (Data)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Vector (Vector)
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)

data Status
  = Empty
  | LastMonth
  | LastWeek
  | Offline UTCTime
  | Online UTCTime
  | Recently
  deriving (Eq, Show, Generic, Data)

data BotInfo = BotInfo
  { canJoinGroups, canReadAllGroupMessages, isInline :: Bool
  , inlineQueryPlaceholder :: Text
  , needLocation :: Bool
  } deriving (Eq, Show, Generic, Data)

data Kind
  = Bot BotInfo
  | Deleted
  | Regular
  | Unknown
  deriving (Eq, Show, Generic, Data)

data Verification
  = Verified
  | Unverified
  deriving (Eq, Show, Enum, Bounded, Ord, Generic, Data)

data Access
  = Accessible
  | Inaccesible
  deriving (Eq, Show, Enum, Bounded, Ord, Generic, Data)

data RestrictionReason = RestrictionReason
  { reason :: Text
  , platforms :: Vector Text
  , description :: Text
  } deriving (Eq, Show, Generic, Data)

data User = User
  { firstName :: Text
  , lastName :: Text
  , username :: Text
  , phoneNumber :: Text
  , status :: Status
  , verificationStatus :: Verification
  , restrictionReason :: Maybe RestrictionReason
  , access :: Access
  , kind :: Kind
  , languageCode :: Maybe Text
  } deriving (Eq, Show, Generic, Data)

concat <$>
  traverse
    (deriveJSON A.defaultOptions)
    [ ''Status
    , ''BotInfo
    , ''Kind
    , ''Verification
    , ''Access
    , ''RestrictionReason
    , ''User
    ]
