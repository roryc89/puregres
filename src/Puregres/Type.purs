module Puregres.Type where

import Prelude

import Control.Apply (lift2)
import Data.Foreign (F, Foreign, readNull)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

newtype Column a = Column
  { serialized :: String
  , table :: Table
  , d :: Foreign -> F a
  }

newtype NullableColumn a = NullableColumn (Column a)

getStr :: forall a. Column a -> String
getStr (Column c) =
  c.serialized

appendStr :: forall a b. Column a -> Column b -> Column a
appendStr (Column a) (Column b) =
  (Column (a {serialized = a.serialized <> ", " <> b.serialized}))

instance columnFunctor :: Functor Column where
  map f (Column c) = Column c {d = functorColumnDecoder f c.d}

functorColumnDecoder :: forall a b. (a -> b) -> (Foreign -> F a) -> (Foreign -> F b)
functorColumnDecoder f dec = map (map f) dec

addNull :: forall a. Decode a => Column a -> Column (NullOrUndefined a)
addNull (Column c) = Column c{d = \f -> f ! c.serialized >>= decode}

addMaybe :: forall a. Decode a => Column a -> Column (Maybe a)
addMaybe (Column c) = Column c{d = \f -> f ! c.serialized >>= readNull >>= traverse decode}

infixl 4 addMaybe as &

instance columnApply :: Apply Column where
  apply (Column col1) (Column col2) =
    Column $ col1 {d = applyColumnDecoder col1.d col2.d}

applyColumnDecoder :: forall a b. (Foreign -> F (a -> b)) -> (Foreign -> F a) -> (Foreign -> F b)
applyColumnDecoder = lift2 apply

instance columnShow :: Show (Column a) where
  show = getStr

andCol :: forall a b. Column (a -> b) -> Column a -> Column b
andCol c0 c1 = appendStr (c0 <*> c1) c1

infixr 3 andCol as &*

newtype Table = Table String

derive instance eqTable :: Eq Table

makeColumn :: forall a. Decode a => Table -> String -> Column a
makeColumn table serialized = Column
  { serialized
  , table: table
  , d: \f -> f ! serialized >>= decode
  }

class ColumnDecode c where
  dec :: Foreign -> F c

-- instance columnDecodeInt :: ColumnDecode Int where
--   dec = decode
-- instance columnDecodeString :: ColumnDecode String where
--   dec = decode
-- instance columnDecodeBoolean :: ColumnDecode Boolean where
--   dec = decode
-- instance columnDecodeNumber :: ColumnDecode Number where
--   dec = decode
-- instance columnDecodeMaybe :: ColumnDecode a => ColumnDecode (Maybe a) where
--   dec f = readNull f >>= traverse decode
-- makeNullableColumn :: forall a. Decode a => Table -> String -> Column (Maybe a)
-- makeNullableColumn table serialized = Column
--   { serialized
--   , table
--   , d: \f -> f ! serialized >>= readNull >>= traverse decode
--   }
