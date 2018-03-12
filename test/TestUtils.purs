module Test.TestUtils where

import Prelude
import Data.Foreign (tagOf, toForeign)
import Puregres.Class (params, class Params)

foreign import unsafeToStringJs :: forall a. a -> String

getParamStrings :: forall a. Params a => a -> Array String
getParamStrings = params >>> (map unsafeToStringJs)

getParamTypes :: forall a. Params a => a -> Array String
getParamTypes = params >>> map (toForeign >>> tagOf)
