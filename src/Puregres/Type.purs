module Puregres.Type where

import Prelude

import Control.Apply (lift2)
import Data.Foreign (F, Foreign, readNull)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

newtype Column a = Column
  { serialized :: String
  , table :: Table
  , d :: Foreign -> F a
  }

newtype NullableColumn a = NullableColumn (Column a)

appendStr :: forall a b. Column a -> Column b -> Column a
appendStr (Column a) col =
  (Column (a {serialized = a.serialized <> ", " <> (show col)}))

instance columnFunctor :: Functor Column where
  map f (Column c) = Column c {d = functorColumnDecoder f c.d}

functorColumnDecoder :: forall a b. (a -> b) -> (Foreign -> F a) -> (Foreign -> F b)
functorColumnDecoder f dec = map (map f) dec

addNull :: forall a. Decode a => Column a -> Column (NullOrUndefined a)
addNull (Column c) = Column c{d = \f -> f ! c.serialized >>= decode}

addMaybe :: forall a. Decode a => Column a -> Column (Maybe a)
addMaybe (Column c) = Column c{d = \f -> f ! c.serialized >>= readNull >>= traverse decode}

instance columnApply :: Apply Column where
  apply (Column col1) (Column col2) =
    Column $ col1 {d = applyColumnDecoder col1.d col2.d}

applyColumnDecoder :: forall a b. (Foreign -> F (a -> b)) -> (Foreign -> F a) -> (Foreign -> F b)
applyColumnDecoder = lift2 apply

instance columnShow :: Show (Column a) where
  show (Column c) = (show c.table) <> "." <> c.serialized

andCol :: forall a b. Column (a -> b) -> Column a -> Column b
andCol c0 c1 = appendStr (c0 <*> c1) c1

infixr 3 andCol as &*

newtype Table = Table String

derive instance eqTable :: Eq Table

instance showTable :: Show Table where
  show (Table s) = s

makeColumn :: forall a. Decode a => Table -> String -> Column a
makeColumn table serialized = Column
  { serialized
  , table: table
  , d: \f -> f ! serialized >>= decode
  }
