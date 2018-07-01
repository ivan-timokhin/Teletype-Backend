{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Telegram.Proxy where

import qualified Telegram.TDLib.API as TDLib

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)

data Socks5Proxy = Socks5Proxy
  { server :: Text
  , port :: Int
  , username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic, Data)

translate :: Socks5Proxy -> TDLib.Proxy
translate (Socks5Proxy s p u pass) = TDLib.ProxySocks5 s (fromIntegral p) u pass

deriveJSON A.defaultOptions ''Socks5Proxy
