module Telegram.TDLib.Example where

import Telegram.TDLib.API (recv, withClient)

example :: IO ()
example =
  withClient $ \client ->
    let loop = do
          update <- recv client 10.0
          case update of
            Just str -> do
              print str
              loop
            Nothing -> pure ()
     in loop
