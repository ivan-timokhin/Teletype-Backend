{-# LANGUAGE ForeignFunctionInterface #-}

module Telegram.TDLib.Bindings
  ( TDLibClient
  , createClient
  , destroyClient
  , sendJSON
  , recvJSON
  , executeJSON
  , setLogFilePath
  , setLogMaxFileSize
  , setLogVerbosityLevel
  ) where

import Data.ByteString (ByteString, packCString, useAsCString)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(CDouble), CInt(CInt), CLLong(CLLong))
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "td_json_client_create" c_td_json_client_create
  :: IO (Ptr ())

foreign import ccall "td_json_client_send" c_td_json_client_send
  :: Ptr () -> CString -> IO ()

foreign import ccall "td_json_client_receive" c_td_json_client_receive
  :: Ptr () -> CDouble -> IO CString

foreign import ccall "td_json_client_execute" c_td_json_client_execute
  :: Ptr () -> CString -> IO CString

foreign import ccall "td_json_client_destroy" c_td_json_client_destroy
  :: Ptr () -> IO ()

foreign import ccall "td_set_log_file_path" c_td_set_log_file_path
  :: CString -> IO CInt

foreign import ccall "td_set_log_max_file_size" c_td_set_log_max_file_size
  :: CLLong -> IO ()

foreign import ccall "td_set_log_verbosity_level" c_td_set_log_verbosity_level
  :: CInt -> IO ()

-- | An instance of a TDLib client
newtype TDLibClient =
  TDLibClient (Ptr ())
  deriving (Eq, Ord)

-- | Creates a new instance of TDLib.
createClient :: IO TDLibClient
createClient = TDLibClient <$> c_td_json_client_create

-- | Sends request to the TDLib client.
--
-- May be called from any thread.
sendJSON ::
     TDLibClient -- ^ The client.
  -> ByteString -- ^ JSON-serialized request to TDLib.
  -> IO ()
sendJSON (TDLibClient client) str =
  useAsCString str $ c_td_json_client_send client

-- | Receives incoming updates and request responses from the TDLib
-- client.
--
-- May be called from any thread, but shouldn't be called
-- simultaneously from two different threads.
recvJSON ::
     TDLibClient -- ^ The client.
  -> Double -- ^ Maximum number of seconds allowed for this function
            -- to wait for new data.
  -> IO (Maybe ByteString) -- ^ JSON-serialized incoming update or
                           -- request response. May be @Nothing@ if
                           -- the timeout expires.
recvJSON (TDLibClient client) timeout = do
  response <- c_td_json_client_receive client (realToFrac timeout)
  if response == nullPtr
    then pure Nothing
    else Just <$> packCString response

-- | Synchronously executes TDLib request.
--
-- May be called from any thread. Only a few requests can be executed
-- synchronously.
executeJSON ::
     TDLibClient -- ^ The client.
  -> ByteString -- ^ JSON-serialized request to TDLib.
  -> IO (Maybe ByteString) -- ^ JSON-serialized null-terminated
                           -- request response. May be @Nothing@ if
                           -- the request can't be parsed.
executeJSON (TDLibClient client) request = do
  response <- useAsCString request (c_td_json_client_execute client)
  if response == nullPtr
    then pure Nothing
    else Just <$> packCString response

-- | Destroys the TDLib client instance.
--
-- After this is called the client instance shouldn't be used anymore.
destroyClient :: TDLibClient -> IO ()
destroyClient (TDLibClient client) = c_td_json_client_destroy client

setLogFilePath :: ByteString -> IO Int
setLogFilePath filename =
  fromIntegral <$> useAsCString filename c_td_set_log_file_path

setLogMaxFileSize :: Integer -> IO ()
setLogMaxFileSize = c_td_set_log_max_file_size . fromInteger

setLogVerbosityLevel :: Int -> IO ()
setLogVerbosityLevel = c_td_set_log_verbosity_level . fromIntegral
