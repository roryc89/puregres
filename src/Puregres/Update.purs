module Puregres.Update where


import Prelude (class Show, show, (<>))
import Puregres.Class (class ColOf, class Params, params)
import Puregres.Comparator (Comparator)
import Puregres.InsertOrUpdateCols (InsertOrUpdateCols)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Where (WHERE(..), WhereExpr(..))

data Update t = Update t (InsertOrUpdateCols t) WHERE

instance paramsUpdate :: Params (Update table) where
  params (Update _ insertCols where') = params insertCols <> params where'

update :: forall t. t -> InsertOrUpdateCols t -> Update t
update table cols =
  Update table cols (WHERE [])

whereVal :: forall col gets colType table a b. Show col => IsSqlValue colType => ColOf col gets colType table =>
  col
  -> Comparator
  -> colType
  -> Update table
  -> Update table
whereVal col comparator val =
   whereExprIntoUpdate (ColVal (show col) comparator (toSql val))

whereExprIntoUpdate :: forall a. WhereExpr -> Update a -> Update a
whereExprIntoUpdate wh (Update t cols (WHERE whs)) = (Update t cols (WHERE (whs <> [wh])))
