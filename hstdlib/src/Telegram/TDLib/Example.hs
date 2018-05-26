module Telegram.TDLib.Example where

import Telegram.TDLib.Bindings (createClient, destroyClient, recvJSON)

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS8

example :: IO ()
example =
  bracket createClient destroyClient $ \client ->
    let loop = do
          update <- recvJSON client 10.0
          case update of
            Just str -> do
              BS8.putStrLn str
              loop
            Nothing -> pure ()
     in loop
