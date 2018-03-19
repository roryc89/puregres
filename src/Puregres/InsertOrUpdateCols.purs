module Puregres.InsertOrUpdateCols where

import Prelude
import Data.Array (mapWithIndex)
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class (class ColOf, class Params, colName)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)

data InsertOrUpdateCols t = InsertOrUpdateCols (Array String) (Array SqlValue)

instance showInsertOrUpdateCols :: Show (InsertOrUpdateCols t) where
  show (InsertOrUpdateCols cols _) =
    "\n  (" <> joinWith ", " cols <> ")"
    <> "\nVALUES"
    <> "\n  ("
    <> (joinWith ", " $ mapWithIndex (\i _ -> "$" <> show (i + 1)) cols)
    <> ")\n"

instance paramsInsertOrUpdateCols :: Params (InsertOrUpdateCols t) where
  params (InsertOrUpdateCols _ p) = p

col :: forall t c gets colType. ColOf c gets colType t => IsSqlValue colType => c /\ colType -> InsertOrUpdateCols t
col (c /\ r) = InsertOrUpdateCols [colName c ] [toSql r]

andCol :: forall table col gets colType. ColOf col gets colType table => Show col => IsSqlValue colType =>
  InsertOrUpdateCols table
  -> col /\ colType
  -> InsertOrUpdateCols table
andCol (InsertOrUpdateCols cs sqls) (c /\ r) = InsertOrUpdateCols newCs newSqls
  where
    newCs = cs <> [colName c]
    newSqls = sqls <> [toSql r]

infixl 3 andCol as ++
