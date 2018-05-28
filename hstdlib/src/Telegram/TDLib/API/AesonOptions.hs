module Telegram.TDLib.API.AesonOptions
  ( aesonOptions
  , objectAesonOptions
  , functionOptions
  ) where

import qualified Data.Aeson as A
import Data.Char (isUpper, toLower)

baseOptions :: A.Options
baseOptions =
  A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '_' . dropWhile (== '_')
    , A.sumEncoding =
        A.TaggedObject {A.tagFieldName = "@type", A.contentsFieldName = "_"}
    , A.allNullaryToStringTag = False
    , A.tagSingleConstructors = True
    }


aesonOptions :: String -> A.Options
aesonOptions prefix = baseOptions {A.constructorTagModifier = (prefix ++)}

objectAesonOptions :: A.Options
objectAesonOptions = A.defaultOptions {A.sumEncoding = A.UntaggedValue}

functionOptions :: A.Options
functionOptions = baseOptions {A.constructorTagModifier = lowercaseFirst}
  where
    lowercaseFirst (c:cs)
      | isUpper c = toLower c : cs
    lowercaseFirst str = str
