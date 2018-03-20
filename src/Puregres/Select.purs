module Puregres.Select where


import Prelude

import Control.Apply (lift2)
import Data.Array (null)
import Data.Foreign (F, Foreign)
import Data.Record.Builder as Builder
import Data.String (joinWith)
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class (class Params, params, class Col, class ColOf, getF)
import Puregres.Comparator (Comparator)
import Puregres.PostgresType (class PostgresType, postgresType)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Type (EndQuery(EndQuery), TABLE(TABLE))
import Puregres.Where (WHERE(..), WhereExpr(..), showWheres)


-- INNER JOIN

data INNER_JOIN a = INNER_JOIN (On a)

instance showINNER_JOIN :: (Show a, Show next) => Show (INNER_JOIN (TABLE a next)) where
  show (INNER_JOIN (On colStr (TABLE a next))) = "\n  INNER JOIN " <> show a <> " ON " <> colStr <> show next


inner_join :: forall a b c. (a -> b) -> (b -> On c) -> a -> INNER_JOIN c
inner_join t o next = INNER_JOIN (o (t next))

data On a = On String a

instance showOn :: Show a => Show (On a) where
  show (On s _) = " ON " <> s

on :: forall a b t.  Show a => Show b =>
  a -> Comparator ->  b -> t -> On t
on a comparator b t = On (show a <> (show comparator) <> show b) t

-- COLUMNS

merge :: forall a b c. Union b a c => { | a } -> { | b } -> { | c }
merge rec1 rec2 = Builder.build (Builder.merge rec1) rec2

data SelectStart gets head tail = SelectStart (Foreign -> F gets) (ColCons head tail)

data ColCons col tail = ColCons col tail

instance showColCons :: (Show col, Show tail) => Show (ColCons col tail) where
  show (ColCons col tail) = show tail <> show col <> ",\n    "

data ColNil = ColNil

instance showColNil :: Show ColNil where
  show ColNil = ""

class ColsInSelectFrom cols table

instance colsInAASelectFromExprAAAColNil :: ColsInSelectFrom ColNil t

instance colsInSelectFromExprColCons :: (ColOf col gets colType table, ColsInSelectFrom next table)
  => ColsInSelectFrom (ColCons col next) table

data SelectFrom cols gets table = SelectFrom (Foreign -> F gets) cols table WHERE ORDER_BY

instance paramsSelectFrom :: Params (SelectFrom cols gets table) where
  params (SelectFrom _ _ _ where' _) = params where'

instance showSelectFrom :: (Show head, Show tail, Show table)
  => Show (SelectFrom (ColCons head tail) gets table) where
    show  = showSELECTWithParamCount 1

showSELECTWithParamCount :: forall head tail gets table. Show head => Show tail => Show table =>
  Int -> (SelectFrom (ColCons head tail) gets table) -> String
showSELECTWithParamCount paramCount (SelectFrom _ (ColCons head tail) table where' orderBy) =
    "SELECT\n    "
      <> show tail
      <> show head
      <> "\nFROM"
      <> show table
      <> showWheres paramCount where'
      <> show orderBy

from :: forall head tail table gets. (ColsInSelectFrom (ColCons head tail) table) =>
  (EndQuery -> table)
  -> SelectStart gets head tail
  -> SelectFrom (ColCons head tail) gets table
from table (SelectStart gets cols) =
  SelectFrom gets cols (table EndQuery) (WHERE []) (ORDER_BY [])

select :: forall head gets colType. Col head gets colType => head -> SelectStart gets head ColNil
select col = SelectStart (getF col) (ColCons col ColNil)

andSelect :: forall tail head newHead getsNew gets r colType.
  Union gets r getsNew => Col newHead { | r } colType =>
  SelectStart { | gets } head tail
  -> newHead
  -> SelectStart { | getsNew } newHead (ColCons head tail)
andSelect  (SelectStart gets cols) col =
  SelectStart
    (\f -> lift2 merge (getF col f) (gets f))
    (ColCons col cols)

infixl 5 andSelect as ++

-- WHERE

whereExprIntoSelectFrom :: forall cols gets table.
  WhereExpr -> SelectFrom cols gets table -> SelectFrom cols gets table
whereExprIntoSelectFrom wh (SelectFrom cols gets table (WHERE whs) orderBy) =
  SelectFrom cols gets table (WHERE (whs <> [wh])) orderBy

whereVal :: forall col cols g gets table colType.
  Show col => IsSqlValue colType => ColOf col g colType table =>
  col
  -> Comparator
  -> colType
  -> SelectFrom cols gets table
  -> SelectFrom cols gets table
whereVal col comparator val =
   whereExprIntoSelectFrom (ColVal (show col) comparator (toSql val))

whereAny :: forall col cols g gets colType table.
  Show col => IsSqlValue colType => ColOf col g colType table => PostgresType (Array colType) =>
  col
  -> Comparator
  -> Array colType
  -> SelectFrom cols gets table
  -> SelectFrom cols gets table
whereAny col comparator vals =
  whereExprIntoSelectFrom (ColAny (show col) comparator (postgresType vals) (map toSql vals))

whereQuery :: forall col cols1 cols2 gets1 gets2 colType table1 table2.
  Show col => Show table2 => Show cols2 => ColOf col gets2 colType table1 =>
  col
  -> Comparator
  -> SelectFrom (ColCons cols2 ColNil) gets2 table2
  -> SelectFrom cols1 gets1 table1
  -> SelectFrom cols1 gets1 table1
whereQuery col comparator selectFrom =
  whereExprIntoSelectFrom (ColValSubQuery str (params selectFrom))
    where
      str i = show col <> " = (\n" <> showSELECTWithParamCount i selectFrom <> ")"

-- ORDER BY

data ORDER_BY = ORDER_BY (Array Order)

data Order = Order String Direction

instance showOrder :: Show Order where
  show (Order str dir) = str <> " " <> (show dir)

data Direction = ASC | DESC

instance showDirection :: Show Direction where
  show ASC = "ASC"
  show DESC = "DESC"

orderExprIntoSelectFrom :: forall cols gets table.
  Order -> SelectFrom cols gets table -> SelectFrom cols gets table
orderExprIntoSelectFrom order (SelectFrom gets cols table where' (ORDER_BY orders)) =
  SelectFrom gets cols table where' (ORDER_BY (orders <> [order]))

instance showORDER_BY :: Show ORDER_BY where
  show (ORDER_BY orders) = if null orders
    then ""
    else "\nORDER BY\n  " <> joinWith ",\n  " (map show orders)

order_by :: forall col cols gets gets2 table colType.
  Show col =>
  ColOf col gets2 colType table =>
  col
  -> Direction
  -> SelectFrom cols gets table
  -> SelectFrom cols gets table
order_by col direction =
  orderExprIntoSelectFrom (Order (show col) direction)

-- END/RUN

data SelectEnded gets = SelectEnded (Foreign -> F gets) String (Array SqlValue)

instance showSelectEnded :: Show (SelectEnded gets) where
  show (SelectEnded _ s _) = s

instance paramsSelectEnded :: Params (SelectEnded gets) where
  params (SelectEnded _ _ ps) = ps

endSelect :: forall head tail gets table. Show head => Show tail => Show table =>
  SelectFrom (ColCons head tail) gets table -> SelectEnded gets
endSelect selectFrom@(SelectFrom gets _ _ _ _) =
  SelectEnded gets (show selectFrom) (params selectFrom)
