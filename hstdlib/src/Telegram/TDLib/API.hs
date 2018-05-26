{-# LANGUAGE TemplateHaskell #-}

module Telegram.TDLib.API
  ( TDLibClient
  , withClient
  , recv
  , createClient
  , destroyClient
  ) where

import Telegram.TDLib.API.AesonOptions (aesonOptions, objectAesonOptions)
import Telegram.TDLib.Bindings
  ( TDLibClient
  , createClient
  , destroyClient
  , recvJSON
  )

import Data.Aeson (eitherDecodeStrict')
import Data.Aeson.TH (deriveJSON)
import Control.Exception (bracket)

withClient :: (TDLibClient -> IO a) -> IO a
withClient = bracket createClient destroyClient

data AuthorizationState =
  WaitTdlibParameters
  deriving (Eq, Show)

deriveJSON (aesonOptions "authorizationState") ''AuthorizationState

data Update = AuthorizationState
  { authorizationState :: AuthorizationState
  } deriving (Eq, Show)

deriveJSON (aesonOptions "update") ''Update

data Object = Update Update
  deriving (Eq, Show)

deriveJSON objectAesonOptions ''Object

recv :: TDLibClient -> Double -> IO (Maybe (Either String Object))
recv client timeout = fmap eitherDecodeStrict' <$> recvJSON client timeout
