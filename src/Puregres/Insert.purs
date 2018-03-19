module Puregres.Insert where

import Prelude
import Puregres.Class (class ColOf, class Col, class Params, colName, params)
import Puregres.InsertOrUpdateCols (InsertOrUpdateCols)
import Puregres.Type (end, EndQuery)

data InsertInto table = InsertInto table (InsertOrUpdateCols table)

instance showInsertInto :: (Show table) => Show (InsertInto table) where
  show (InsertInto table cols) =
    "INSERT INTO"
    <> show table
    <> show cols

instance paramsInsertInto :: Params (InsertInto table) where
  params (InsertInto _ insertCols) = params insertCols

data InsertIntoReturning table col = InsertIntoReturning table (InsertOrUpdateCols table) col

instance showInsertIntoReturning :: (Show table, Col col gets colType) => Show (InsertIntoReturning table col) where
  show (InsertIntoReturning t cols returning) =
    "INSERT INTO"
    <> show t
    <> show cols
    <> "RETURNING "
    <> colName returning

instance paramsInsertIntoReturning :: Params (InsertIntoReturning table col) where
  params (InsertIntoReturning _ insertCols _) = params insertCols

insertInto :: forall table. (EndQuery -> table) -> InsertOrUpdateCols table -> InsertInto table
insertInto table into =
  InsertInto (table end) into

returning :: forall table col gets colType . ColOf col gets colType table => col -> InsertInto table -> InsertIntoReturning table col
returning ret (InsertInto t cols) =
  InsertIntoReturning t cols ret
