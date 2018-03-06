module Puregres.Type where

import Prelude

import Control.Apply (lift2)
import Data.Foreign (F, Foreign, readNull)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Puregres.PuregresSqlValue (class IsSqlValue, decode_)

newtype Column a = Column
  { name :: String
  , table :: Table
  , d :: Foreign -> F a
  }

makeColumn :: forall a. IsSqlValue a => Table -> String -> Column a
makeColumn table name = Column
  { name
  , table: table
  , d: \f -> f ! name >>= decode_
  }

instance columnFunctor :: Functor Column where
  map f (Column c) = Column c {d = functorColumnDecoder f c.d}

functorColumnDecoder :: forall a b. (a -> b) -> (Foreign -> F a) -> (Foreign -> F b)
functorColumnDecoder f dec = map (map f) dec

instance columnApply :: Apply Column where
  apply (Column col1) (Column col2) =
    Column $ col1 {d = applyColumnDecoder col1.d col2.d}

applyColumnDecoder :: forall a b. (Foreign -> F (a -> b)) -> (Foreign -> F a) -> (Foreign -> F b)
applyColumnDecoder = lift2 apply

instance columnShow :: Show (Column a) where
  show (Column c) = (show c.table) <> "." <> c.name

addMaybe :: forall a. IsSqlValue a => Column a -> Column (Maybe a)
addMaybe (Column c) = Column c{d = \f -> f ! c.name >>= readNull >>= traverse decode_}

appendStr :: forall a b. Column a -> Column b -> Column a
appendStr (Column a) col =
  (Column (a {name = a.name <> ", " <> (show col)}))

andCol :: forall a b. Column (a -> b) -> Column a -> Column b
andCol c0 c1 = appendStr (c0 <*> c1) c1

newtype Table = Table String

derive instance eqTable :: Eq Table

instance showTable :: Show Table where
  show (Table s) = s
