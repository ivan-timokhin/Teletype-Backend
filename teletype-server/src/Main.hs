{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Proxy (Proxy)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Options.Applicative as O
import System.IO (FilePath)
import Control.Concurrent.Async (link)

import qualified Network.Wai.Handler.Warp as W
import Servant

import qualified Data.Telegram.Parameters as T
import qualified Data.Telegram.Proxy as T
import qualified Data.Telegram.User as T
import qualified Telegram.Reactor as T

type TeletypeAPI
   = "contacts" :> Get '[ JSON] (Vector T.User)
   :<|> "auth_code" :> ReqBody '[ PlainText] Text :> Post '[ JSON] Bool

teletypeAPI :: Proxy TeletypeAPI
teletypeAPI = Proxy

server :: ServerT TeletypeAPI (ReaderT T.Reactor Handler)
server = contacts :<|> checkAuthCode
  where
    contacts :: ReaderT T.Reactor Handler (Vector T.User)
    contacts = do
      reactor <- ask
      userIds <- T.searchContacts' reactor "" 500
      T.lookupUsers reactor userIds
    checkAuthCode :: Text -> ReaderT T.Reactor Handler Bool
    checkAuthCode code = do
      reactor <- ask
      T.checkAuthCode' reactor Nothing code
      pure True

server' :: Server TeletypeAPI
server' = contacts :<|> checkAuthCode
  where
    contacts :: Handler (Vector T.User)
    contacts = do
      liftIO $ putStrLn "Requesting contacts"
      pure []
    checkAuthCode code = do
      liftIO $ putStrLn $ "Checking code " ++ show code
      pure True

main :: IO ()
main = do
  opts <- O.execParser optsP
  config <- C.load [C.Required (configFile opts)]
  params <- lookupParameters config
  print params
  --W.run 8080 $ serve teletypeAPI server'
  T.withReactor params $ \reactor -> do
    link (T.eventLoopThread reactor)
    W.run 8080 $
      serve teletypeAPI $
      hoistServer teletypeAPI (flip runReaderT reactor) server
  where
    optsP =
      O.info
        (O.helper <*> clargs)
        (O.fullDesc <>
         O.progDesc
           "Run a server that reports the contact list of the specified contact" <>
         O.header "A Teletype project server")

newtype CLArgs = CLArgs
  { configFile :: FilePath
  } deriving (Show, Eq, Ord)

clargs :: O.Parser CLArgs
clargs =
  CLArgs <$>
  O.strOption
    (O.long "config" <> O.short 'c' <> O.metavar "CONFIG" <>
     O.help "Location of the configuration file" <>
     O.value "teletype-server.cfg" <>
     O.showDefault)

lookupParameters :: C.Config -> IO T.Parameters
lookupParameters config =
  T.Parameters <$> require "telegram.phone-number" <*> pure T.Production <*>
  requireStorage <*>
  pure False <*>
  requireAPI <*>
  requireSysinfo <*>
  pure "Teletype 0.0.1" <*>
  lookupProxyConfig config "telegram.proxy" <*>
  require "telegram.timeout"
  where
    require :: C.Configured a => C.Name -> IO a
    require = C.require config
    requireAPI :: IO T.API
    requireAPI =
      T.API <$> require "telegram.api.id" <*> require "telegram.api.hash"
    requireStorage =
      T.Storage <$> require "telegram.storage.database-directory" <*>
      require "telegram.storage.files-directory" <*>
      require "telegram.storage.files-database" <*>
      require "telegram.storage.chat-info-database" <*>
      require "telegram.storage.message-database" <*>
      pure True <*>
      pure True <*>
      require "telegram.storage.encryption-key"
    requireSysinfo =
      T.SystemInfo <$> require "telegram.system.language-code" <*>
      require "telegram.system.device" <*>
      require "telegram.system.version"

lookupProxyConfig :: C.Config -> C.Name -> IO (Maybe T.Socks5Proxy)
lookupProxyConfig config name =
  runMaybeT $
  T.Socks5Proxy <$> lookupIn "server" <*> lookupIn "port" <*>
  lookupIn "username" <*>
  lookupIn "password"
  where
    lookupIn :: C.Configured a => C.Name -> MaybeT IO a
    lookupIn field = MaybeT $ C.lookup config (name <> "." <> field)
