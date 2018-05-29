module Telegram.TDLib.API.AesonOptions
  ( namedCtors
  , anonymousCtors
  , prefixedCtors
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


prefixedCtors :: String -> A.Options
prefixedCtors prefix = baseOptions {A.constructorTagModifier = (prefix ++)}

anonymousCtors :: A.Options
anonymousCtors = A.defaultOptions {A.sumEncoding = A.UntaggedValue}

namedCtors :: A.Options
namedCtors = baseOptions {A.constructorTagModifier = lowercaseFirst}
  where
    lowercaseFirst (c:cs)
      | isUpper c = toLower c : cs
    lowercaseFirst str = str
