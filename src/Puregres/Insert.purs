module Puregres.Insert where

import Prelude
import Data.Array (mapWithIndex, length)
import Data.Foldable (foldr, foldl)
import Data.String (joinWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Foreign (Foreign, F)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class (class ColOf, class Column, class Params, colName, params)
import Puregres.InsertOrUpdateCols (InsertOrUpdateCols(..))
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Type (end, EndQuery)

data InsertInto t = InsertInto t (InsertOrUpdateCols t)

instance showInsertInto :: (Show t) => Show (InsertInto t) where
  show (InsertInto t cols) =
    "INSERT INTO"
    <> show t
    <> show cols

instance paramsInsertInto :: Params (InsertInto t) where
  params (InsertInto _ insertCols) = params insertCols

data InsertIntoReturning t c = InsertIntoReturning t (InsertOrUpdateCols t) c

instance showInsertIntoReturning :: (Show t, Column c r) => Show (InsertIntoReturning t c) where
  show (InsertIntoReturning t cols returning) =
    "INSERT INTO"
    <> show t
    <> show cols
    <> "RETURNING "
    <> colName returning

instance paramsInsertIntoReturning :: Params (InsertIntoReturning t c) where
  params (InsertIntoReturning _ insertCols _) = params insertCols

insertInto :: forall t. (EndQuery -> t) -> InsertOrUpdateCols t -> InsertInto t
insertInto table into =
  InsertInto (table end) into

returning :: forall t c r. ColOf c t r => c -> InsertInto t -> InsertIntoReturning t c
returning ret (InsertInto t cols) =
  InsertIntoReturning t cols ret
