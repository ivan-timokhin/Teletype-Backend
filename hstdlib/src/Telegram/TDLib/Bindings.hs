{-# LANGUAGE ForeignFunctionInterface #-}

module Telegram.TDLib.Bindings where

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(CDouble))
import Foreign.Ptr (Ptr)

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
