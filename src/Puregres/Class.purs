module Puregres.Class where

import Database.Postgres.SqlValue (SqlValue)

class Params a where
  params :: a -> Array SqlValue
