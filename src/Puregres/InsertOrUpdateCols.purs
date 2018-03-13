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

col :: forall t c r. ColOf c t r => IsSqlValue r => c /\ r -> InsertOrUpdateCols t
col (c /\ r) = InsertOrUpdateCols [colName c ] [toSql r]

andCol :: forall t c r. ColOf c t r => Show c => IsSqlValue r =>
  InsertOrUpdateCols t
  -> c /\ r
  -> InsertOrUpdateCols t
andCol (InsertOrUpdateCols cs sqls) (c /\ r) = InsertOrUpdateCols newCs newSqls
  where
    newCs = cs <> [colName c]
    newSqls = sqls <> [toSql r]

infixl 3 andCol as ++
