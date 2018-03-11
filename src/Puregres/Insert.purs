module Puregres.Insert where

import Prelude
import Data.Array (mapWithIndex)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Type (Table(..), Column(..), ColumnWoDecoder(..), removeDecoder)
import Puregres.Where (WHERE(..))


data INSERT = INSERT Table ColsAndVals WHERE

val :: forall a. (IsSqlValue a) => Column a -> a -> ColAndVal
val column value = ColAndVal (removeDecoder column) (toSql value)

insertInto :: Table -> Array ColAndVal -> INSERT
insertInto table cols = INSERT table (ColsAndVals cols) (WHERE [])

instance showINSERT :: Show INSERT where
  show (INSERT table colAndVals (WHERE wheres)) =
    "INSERT INTO "
      <> show table
      <> "\n"
      <> show colAndVals

data ColAndVal = ColAndVal ColumnWoDecoder SqlValue

data ColsAndVals = ColsAndVals (Array ColAndVal)

instance paramsINSERT :: Params INSERT where
  params (INSERT _  colsAndVals where_) = params colsAndVals <> params where_

instance showColsAndVals :: Show ColsAndVals where
  show (ColsAndVals values) =
    "(" <> (joinWith ", " $ showCol <$> values) <> ")"
    <> "\nVALUES\n"
    <>  "(" <> (joinWith ", " $ mapWithIndex showValue values) <> ")"
    where
      showCol (ColAndVal (ColumnWoDecoder {name}) _) = name
      showValue i _ = "$" <> show i

instance paramsColsAndVals :: Params ColsAndVals where
  params (ColsAndVals colsAndVals) = map (\(ColAndVal _ val) -> val) colsAndVals
