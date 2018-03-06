module Puregres.Where (WHERE(..), WhereExpr(..)) where

import Database.Postgres.SqlValue (SqlValue)

newtype WHERE = WHERE (Array WhereExpr)

data WhereExpr
  = ColEq String SqlValue
  | ColNull String
  | ColEqSubQuery (Int -> String) (Array SqlValue)
