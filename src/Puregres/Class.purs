module Puregres.Class where

import Data.Foreign

import Database.Postgres.SqlValue (SqlValue)
import Prelude (class Show)

class (Show col) <= Col col gets colType | col -> gets colType where
  getF :: col -> Foreign -> F gets
  colName :: col -> String

class (Col col gets colType) <= ColOf col gets colType table | col table -> gets colType

class Params a where
  params :: a -> Array SqlValue

  -- class Column column res | column -> res where
  --   colName :: column -> String
  --
  -- class (Column column res) <= ColOf column table res | column table -> res where
  --   from_ :: column -> table -> Foreign -> F res
