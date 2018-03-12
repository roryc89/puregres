module Puregres.Select where

import Control.Apply (lift2)
import Data.Array (null)
import Data.Foreign (Foreign, F)
import Data.Foreign.Index ((!))
import Data.String (joinWith)
import Database.Postgres.SqlValue (SqlValue)
import Prelude
import Puregres.Class (params, class Params)
import Puregres.Where (WHERE(..), WhereExpr(..), showWheres)
import Puregres.PuregresSqlValue (decode_, class IsSqlValue, toSql)

data TABLE t next = TABLE t next

instance showTABLE :: (Show a, Show b) => Show (TABLE a b) where
  show (TABLE a b) = " " <> show a <> show b

data INNER_JOIN a = INNER_JOIN String String (On a)

data LEFT_JOIN a = LEFT_JOIN (On a)

instance showINNER_JOIN :: (Show a) => Show (INNER_JOIN a) where
  show (INNER_JOIN t next on) = "\n  INNER JOIN" <> t <> show on <> next

data On a = On String a

instance showOn :: Show a => Show (On a) where
  show (On s _) = " ON " <> s

on :: forall a b res t.
  Column a res => Column b res => ColOf b t res => Show a => Show b =>
  a -> b -> t -> On t
on a b t = On (show a <> " = " <> show b) t

instance showLEFT_JOIN :: (Show a) => Show (LEFT_JOIN a) where
  show (LEFT_JOIN a) = "\n  LEFT JOIN" <> show a

class Column c res | c -> res

class (Column c res) <= ColOf c t res | c t -> res where
  from_ :: c -> t -> Foreign -> F res

data SELECT tableExpr fn = SELECT (Array String) tableExpr WHERE ORDER_BY fn

instance showSELECT :: (Show tableExpr) => Show (SELECT tableExpr fn) where
  show = showSELECTWithParamCount true 1

showSELECTWithParamCount :: forall t f. (Show t) =>
  Boolean -> Int -> SELECT t f -> String
showSELECTWithParamCount aliasColumns paramCount (SELECT shownCols tableExpr where_ orderBy _) =
    "SELECT\n    "
      <> colString
      <> "\nFROM"
      <> show tableExpr
      <> showWheres paramCount where_
      <> show orderBy
    where
      colString = shownCols
        # map (\c -> if aliasColumns then c <> " as \"" <> c <> "\"" else c)
        # joinWith ",\n    "

intoCol :: forall col table res1 res2. ColOf col table res1 => Show col =>
  SELECT table (res1 -> res2)
  -> col
  -> SELECT table (Foreign -> F res2)
intoCol (SELECT shownCols tableExpr w orderBy fn) col =
  SELECT
    (shownCols <> [show col])
    tableExpr
    w
    orderBy
    (map (map fn) (from_ col tableExpr))

infixl 3 intoCol as .>>

anotherIntoCol :: forall col table res1 res2. ColOf col table res1 => Show col =>
  SELECT table (Foreign -> F (res1 -> res2))
  -> col
  -> SELECT table (Foreign -> F res2)
anotherIntoCol (SELECT shownCols tableExpr w orderBy fn) col =
  SELECT (shownCols <> [show col]) tableExpr w orderBy ((lift2 apply) fn (from_ col tableExpr))

infixl 2 anotherIntoCol as >>

whereIntoSELECT :: forall a b.  WhereExpr -> SELECT a b -> SELECT a b
whereIntoSELECT wh (SELECT shownCols a (WHERE whs) ord b) =
  SELECT shownCols a (WHERE (whs <> [wh])) ord b

whereExprIntoFROM :: forall a. WhereExpr -> FROM a -> FROM a
whereExprIntoFROM wh (FROM a (WHERE whs) orderBy) = (FROM a (WHERE (whs <> [wh])) orderBy)

data FROM t = FROM t WHERE ORDER_BY

whereVal :: forall col t val a b. Show col => IsSqlValue val => ColOf col t val =>
  col
  -> val
  -> FROM t
  -> FROM t
whereVal col val =
   whereExprIntoFROM (ColEq (show col) (toSql val))

whereSub :: forall col1 col2 t1 t2 val a b.
  Show col1 => Show col2 => Show t2 => ColOf col1 t1 val => ColOf col2 t2 val =>
  col1
  -> col2
  -> FROM t2
  -> FROM t1
  -> FROM t1
whereSub col1 col2 (FROM t wheres orderBys) =
   whereExprIntoFROM (ColEqSubQuery str (params wheres))
     where
       sel = SELECT [show col2] t wheres orderBys unit
       str i = show col1 <> " = (\n" <> showSELECTWithParamCount false i sel <> ")"

from :: forall t. t -> FROM t
from t = FROM t (WHERE []) (ORDER_BY [])

fromC :: forall t133 t134. t134 -> t133 -> SELECT t134 t133
fromC = from >>> cols_

cols_ :: forall t b . FROM t -> b -> SELECT t b
cols_ (FROM t where_ orderBy) b = SELECT [] t where_ orderBy b

-- inner_join :: forall a b c. (a -> b) -> (b -> On c) -> a -> INNER_JOIN c
inner_join :: forall t125 t126. Show t126 => (EndQuery -> t126) -> (t126 -> On t125) -> EndQuery -> INNER_JOIN t125
inner_join t o next = INNER_JOIN (show (t end)) (show next) (o (t next))

data SelectQuery a = SelectQuery String (Array SqlValue) (Foreign -> F a)

instance showSelectQuery :: Show (SelectQuery a) where
  show (SelectQuery s _ _) = s

instance paramsSelectQuery :: Params (SelectQuery a) where
  params (SelectQuery _ p _) = p

select :: forall a b. Show a => SELECT a (Foreign -> F b) -> SelectQuery b
select sel@(SELECT shownCols t where_ orderBy b) =
  SelectQuery
    (show sel)
    (params where_)
    b

-- ORDER BY

data ORDER_BY = ORDER_BY (Array Order)

data Order = Order String Direction

instance showOrder :: Show Order where
  show (Order str dir) = str <> " " <> (show dir)

data Direction = ASC | DESC

instance showDirection :: Show Direction where
  show ASC = "ASC"
  show DESC = "DESC"

orderExprIntoFROM :: forall a. Order -> FROM a -> FROM a
orderExprIntoFROM order (FROM a where_ (ORDER_BY orders)) =
 FROM a where_ (ORDER_BY (orders <> [order]))

instance showORDER_BY :: Show ORDER_BY where
  show (ORDER_BY orders) = if null orders
    then ""
    else "\nORDER BY\n  " <> joinWith ",\n  " (map show orders)

order_by :: forall col t val.
  Show col => ColOf col t val =>
  col
  -> Direction
  -> FROM t
  -> FROM t
order_by col direction =
  orderExprIntoFROM (Order (show col) direction)

-- end

data EndQuery = EndQuery

end :: EndQuery
end = EndQuery

instance showEndQuery :: Show EndQuery where
  show _ = ""

-- instance helper functions

fromTable :: forall b a c d. ColOf a c d => a -> TABLE b c -> Foreign -> F d
fromTable t (TABLE a b) = from_ t b

fromInnerJoin :: forall a b c. ColOf a b c => a -> INNER_JOIN b -> Foreign -> F c
fromInnerJoin t (INNER_JOIN _ _ (On _ a)) = from_ t a

getPropAndDecode :: forall t a res. Show t => IsSqlValue res => t -> a -> Foreign -> F res
getPropAndDecode t a f = f ! (show t) >>= decode_
