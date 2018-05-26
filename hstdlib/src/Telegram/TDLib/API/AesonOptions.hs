module Telegram.TDLib.API.AesonOptions
  ( aesonOptions
  , objectAesonOptions
  ) where

import qualified Data.Aeson as A

aesonOptions :: String -> A.Options
aesonOptions prefix =
  A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '_' . dropWhile (== '_')
    , A.constructorTagModifier = (prefix ++)
    , A.sumEncoding =
        A.TaggedObject {A.tagFieldName = "@type", A.contentsFieldName = "_"}
    , A.allNullaryToStringTag = False
    , A.tagSingleConstructors = True
    }

objectAesonOptions :: A.Options
objectAesonOptions = A.defaultOptions {A.sumEncoding = A.UntaggedValue}
