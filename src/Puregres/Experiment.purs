module Puregres.Experiment where

import Control.Apply (apply, lift2)
import Data.Foreign (Foreign, F)
import Data.String (joinWith)
import Prelude (class Show, flip, map, show, unit, (<>))
import Puregres.Class (params)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)

import Puregres.Where (WHERE(..), WhereExpr(..), showWheres)

data TABLE t next = TABLE t next

instance showTABLE :: (Show a, Show b) => Show (TABLE a b) where
  show (TABLE a b) = show a <> show b

data INNER_JOIN a = INNER_JOIN (On a)

data LEFT_JOIN a = LEFT_JOIN (On a)

instance showINNER_JOIN :: (Show a) => Show (INNER_JOIN a) where
  show (INNER_JOIN on) = " INNER_JOIN" <> show on

data On a = On String a

instance showOn :: Show a => Show (On a) where
  show (On s a) = " On " <> s <> show a

on :: forall a b res t.
  Column a res => Column b res => ColOf b t res => Show a => Show b =>
  a -> b -> t -> On t
on a b t = On (show a <> "=" <> show b) t

instance showLEFT_JOIN :: (Show a) => Show (LEFT_JOIN a) where
  show (LEFT_JOIN a) = " LEFT_JOIN" <> show a

class Column c res | c -> res

class (Column c res) <= ColOf c t res | c t -> res where
  from_ :: c -> t -> Foreign -> F res

data SELECT tableExpr fn = SELECT (Array String) tableExpr WHERE fn

instance showSELECT :: (Show tableExpr) => Show (SELECT tableExpr fn) where
  show = showSELECTWithParamCount 1

showSELECTWithParamCount :: forall t f. (Show t) =>
  Int -> SELECT t f -> String
showSELECTWithParamCount paramCount (SELECT showCols tableExpr where_ _) =
    "SELECT "
      <> joinWith ", " showCols
      <> show tableExpr
      <> showWheres paramCount where_

intoCol :: forall col table res1 res2 wh. ColOf col table res1 => Show col =>
  SELECT table (res1 -> res2)
  -> col
  -> SELECT table (Foreign -> F res2)
intoCol (SELECT shownCols tableExpr w fn) col =
  SELECT (shownCols <> [show col]) tableExpr w (map (map fn) (from_ col tableExpr))

infixl 3 intoCol as >>>

anotherIntoCol :: forall col table res1 res2 wh. ColOf col table res1 => Show col =>
  SELECT table (Foreign -> F (res1 -> res2))
  -> col
  -> SELECT table (Foreign -> F res2)
anotherIntoCol (SELECT shownCols tableExpr w fn) col =
  SELECT (shownCols <> [show col]) tableExpr w ((lift2 apply) fn (from_ col tableExpr))

infixl 2 anotherIntoCol as >>

whereIntoFrom :: forall a b. SELECT a b -> WHERE -> SELECT a b
whereIntoFrom (SELECT shownCols a w1 b) w2 = SELECT shownCols a (w1 <> w2) b

whereExprIntoFROM :: forall a. WhereExpr -> FROM a -> FROM a
whereExprIntoFROM wh (FROM a (WHERE whs)) = (FROM a (WHERE (whs <> [wh])))

data FROM t = FROM t WHERE

whereVal :: forall col t val . Show col => IsSqlValue val => ColOf col t val =>
  col
  -> val
  -> FROM t
  -> FROM t
whereVal col val =
   whereExprIntoFROM (ColEq (show col) (toSql val))

whereSub :: forall col1 col2 t1 t2 val.
  Show col2 => Show t2 => ColOf col1 t1 val => ColOf col2 t2 val =>
  col1
  -> col2
  -> FROM t2
  -> FROM t1
  -> FROM t1
whereSub col1 col2 from@(FROM t wheres) =
   whereExprIntoFROM (ColEqSubQuery ((flip showSELECTWithParamCount) select) (params wheres))
     where select = SELECT [show col2] t wheres unit

from :: forall t65. t65 -> FROM t65
from  t = FROM t (WHERE [])

cols_ :: forall t b . FROM t -> b -> SELECT t b
cols_ (FROM t where_) b = SELECT [] t where_ b

inner_join :: forall a b c. (a -> b) -> (b -> On c) -> a -> INNER_JOIN c
inner_join t on next = INNER_JOIN (on (t next))

data SelectQuery a = SelectQuery String (Foreign -> F a)

select :: forall a b. Show a => SELECT a (Foreign -> F b) -> SelectQuery b
select (SELECT shownCols a w b) = SelectQuery (colString <> show a) b
  where colString = joinWith ", " shownCols
