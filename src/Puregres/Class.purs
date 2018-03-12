module Puregres.Class where

import Data.Foreign
import Database.Postgres.SqlValue (SqlValue)

class Column column res | column -> res where
  colName :: column -> String

class (Column column res) <= ColOf column table res | column table -> res where
  from_ :: column -> table -> Foreign -> F res

class Params a where
  params :: a -> Array SqlValue
