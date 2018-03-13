module Puregres.Update where


import Prelude (class Show, show, (<>))
import Puregres.Class (class ColOf)
import Puregres.InsertOrUpdateCols (InsertOrUpdateCols(..))
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Where (WHERE(..), WhereExpr(..))


data Update t = Update t (InsertOrUpdateCols t) WHERE

update :: forall t c r. ColOf t c r => t -> InsertOrUpdateCols t -> Update t
update table cols =
  Update table cols (WHERE [])

whereVal :: forall col t val a b. Show col => IsSqlValue val => ColOf col t val =>
  col
  -> val
  -> Update t
  -> Update t
whereVal col val =
   whereExprIntoUpdate (ColEq (show col) (toSql val))

whereExprIntoUpdate :: forall a. WhereExpr -> Update a -> Update a
whereExprIntoUpdate wh (Update t cols (WHERE whs)) = (Update t cols (WHERE (whs <> [wh])))
