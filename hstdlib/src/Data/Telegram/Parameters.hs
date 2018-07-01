{-# LANGUAGE RecordWildCards #-}
module Data.Telegram.Parameters where

import Data.Telegram.Proxy (Socks5Proxy)
import qualified Data.Telegram.Proxy as Proxy
import qualified Telegram.TDLib.API as TDLib

import Data.Text (Text)
import qualified Data.Text as T
import System.IO (FilePath)

data Environment
  = Test
  | Production
  deriving (Eq, Ord, Show, Bounded, Enum)

data API = API
  { apiId :: Int
  , apiHash :: Text
  }

data SystemInfo = SystemInfo
  { languageCode :: Text
  , device :: Text
  , systemVersion :: Text
  }

data Storage = Storage
  { dbDir :: FilePath
  , filesDir :: FilePath
  , useFileDB, useChatInfoDB, useMessageDB, enableOptimizer, ignoreFileNames :: Bool
  , encryptionKey :: Text
  }

data Parameters = Parameters
  { phoneNumber :: Text
  , environment :: Environment
  , storage :: Storage
  , useSecretChats :: Bool
  , api :: API
  , system :: SystemInfo
  , appVersion :: Text
  , proxy :: Maybe Socks5Proxy
  , receptionTimeout :: Double
  }

tdlibParams :: Parameters -> TDLib.Parameters
tdlibParams Parameters {..} =
  TDLib.Parameters
    { TDLib.useTestDC = environment == Test
    , TDLib.databaseDirectory = T.pack (dbDir storage)
    , TDLib.filesDirectory = T.pack (filesDir storage)
    , TDLib.useFileDatabase = useFileDB storage
    , TDLib.useChatInfoDatabase = useChatInfoDB storage
    , TDLib.useMessageDatabase = useMessageDB storage
    , TDLib.useSecretChats = useSecretChats
    , TDLib.apiID = fromIntegral $ apiId api
    , TDLib.apiHash = apiHash api
    , TDLib.systemLanguageCode = languageCode system
    , TDLib.deviceModel = device system
    , TDLib.systemVersion = systemVersion system
    , TDLib.applicationVersion = appVersion
    , TDLib.enableStorageOptimizer = enableOptimizer storage
    , TDLib.ignoreFileNames = ignoreFileNames storage
    }

tdlibProxy :: Parameters -> Maybe TDLib.Proxy
tdlibProxy = fmap Proxy.translate . proxy
