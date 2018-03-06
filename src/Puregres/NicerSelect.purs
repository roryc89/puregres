module Puregres.NicerSelect where

import Prelude

import Control.Apply (lift2)
import Data.Array (all, catMaybes, length, null, uncons)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readNull)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.List.Lazy.NonEmpty (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Database.Postgres.SqlValue (SqlValue)
import Puregres.PuregresSqlValue (class IsSqlValue, decode_, toSql)
import Puregres.Type (Table(..))

newtype Column a = Column
  { name :: String
  , table :: Table
  , d :: Foreign -> F a
  }

makeColumn :: forall a. IsSqlValue a => Table -> String -> Column a
makeColumn table name = Column
  { name
  , table: table
  , d: \f -> f ! name >>= decode_
  }

instance columnFunctor :: Functor Column where
  map f (Column c) = Column c {d = functorColumnDecoder f c.d}

functorColumnDecoder :: forall a b. (a -> b) -> (Foreign -> F a) -> (Foreign -> F b)
functorColumnDecoder f dec = map (map f) dec

instance columnApply :: Apply Column where
  apply (Column col1) (Column col2) =
    Column $ col1 {d = applyColumnDecoder col1.d col2.d}

applyColumnDecoder :: forall a b. (Foreign -> F (a -> b)) -> (Foreign -> F a) -> (Foreign -> F b)
applyColumnDecoder = lift2 apply

instance columnShow :: Show (Column a) where
  show (Column c) = (show c.table) <> "." <> c.name

addMaybe :: forall a. IsSqlValue a => Column a -> Column (Maybe a)
addMaybe (Column c) = Column c{d = \f -> f ! c.name >>= readNull >>= traverse decode_}

newtype SelectInto a = SelectInto a

data FROM
  = LEFT_JOIN FROM On
  | INNER_JOIN FROM On
  | TABLE Table

instance showFROM :: Show FROM where
  show (LEFT_JOIN sel on) = show sel <> "\n  LEFT JOIN" <> show on
  show (INNER_JOIN sel on) = show sel <> "\n  INNER JOIN" <> show on
  show (TABLE t) = " " <> (show t)

data On = On String FROM

instance onShow :: Show On where
  show (On str sel) = show sel <> " " <> str

on :: forall a. Table -> EqC -> On
on s (EqC c1 c2) = On ("ON " <> showColumnWoDecoder c1 <> " = " <> showColumnWoDecoder c2) (TABLE s)

data EqC = EqC ColumnWoDecoder ColumnWoDecoder

eqC :: forall a. Column a -> Column a -> EqC
eqC (Column col1) (Column col2) = EqC {name: col1.name, table:col1.table} {name: col2.name, table:col2.table}

instance eqColumnEqC :: EqColumn (Column a) (Column a) EqC where
  eqColumn = eqC

data ColGroup a = ColGroup ColumnsWoDecoders Tables NullableTables (Column a)

instance showColGroup :: Show (ColGroup a) where
  show (ColGroup columnsWoDecoders _ _ _) = joinWith "," $
     map (\c -> "\n    " <> show c.table <> "." <> c.name) columnsWoDecoders

type ColumnsWoDecoders = Array ColumnWoDecoder

type ColumnWoDecoder =  {name :: String, table :: Table}

showColumnWoDecoder :: ColumnWoDecoder -> String
showColumnWoDecoder c = show c.table <> "." <> c.name

type NullableTables = Array Table

type Tables = Array Table

class SelectExpr old expr new where
  combine :: old -> expr -> new

col :: forall a b. Column a -> SelectInto (a -> b) ->  ColGroup b
col c@(Column column) (SelectInto f) =
  ColGroup [{name: column.name, table:column.table}] [column.table] [] (map f c)

colM :: forall a b. IsSqlValue b => Column a -> SelectInto (a -> b) ->  ColGroup (Maybe b)
colM c@(Column column) (SelectInto f) =
  ColGroup [{name: column.name, table:column.table}] [] [column.table] (addMaybe (map f c))

colIntoColGroup :: forall a b. ColGroup (a -> b) -> Column a -> ColGroup b
colIntoColGroup (ColGroup columnsWoDecoders tables nullableTables column) c@(Column newColumn) =
  ColGroup
    (columnsWoDecoders <> [{name: newColumn.name, table: newColumn.table}])
    (tables <> [newColumn.table])
    nullableTables
    (column <*> c)

colIntoColGroupM :: forall a b. IsSqlValue a => ColGroup (Maybe a -> b) -> Column a -> ColGroup b
colIntoColGroupM (ColGroup columnsWoDecoders tables nullableTables column) c@(Column newColumn) =
  ColGroup
    (columnsWoDecoders <> [{name: newColumn.name, table: newColumn.table}])
    tables
    (nullableTables <> [newColumn.table])
    (column <*> (addMaybe c))

fromInternal :: forall a. FROM -> ColGroup a -> Either String (SELECT a)
fromInternal from_ colGroup@(ColGroup names tables nullableTables column) =
  let
    columnsNotFoundInTables =
      (map (reasonTableNotValidInFrom false from_) tables)
        <> (map (reasonTableNotValidInFrom true from_) nullableTables)
      # catMaybes
  in
    case columnsNotFoundInTables of
      [] -> Right (SELECT from_ (WHERE []) (ORDER_BY []) colGroup)
      arr -> Left (show arr)

from_ :: forall a. Table -> ColGroup a -> Either String (SELECT a)
from_ t = fromInternal (TABLE t)

from :: forall a. Table -> (FROM -> FROM) -> ColGroup a -> Either String (SELECT a)
from table toFrom =
  fromInternal (toFrom (TABLE table))

reasonTableNotValidInFrom :: forall a. Boolean -> FROM -> Table -> Maybe String -- string is reason for not being valid
reasonTableNotValidInFrom isMaybeTable from table = case (getIsTableInFrom table from) of
    [isMaybe] -> if isMaybeTable == isMaybe
      then Nothing
      else Just $ if isMaybeTable
        then "Searching for a maybe table but non maybe table found. Try adding ? or M to the column combinator. Table: " <> show table
        else "Searching for a non maybe table but maybe table found. Try removing ? or M from the column combinator. Table: " <> show table

    [] -> Just $ "Table not found in FROM: " <> show table
    _ -> Just $ "Table found multiple times in FROM: " <> show table

type IsMaybeTable = Boolean

getIsTableInFrom_ :: Boolean -> Table -> FROM -> Array (IsMaybeTable)
getIsTableInFrom_ isMaybe table from =
  case from of
    LEFT_JOIN froml (On _ fromr) -> (getIsTableInFrom_ isMaybe table froml) <> (getIsTableInFrom_ true table fromr)
    INNER_JOIN froml (On _ fromr) -> (getIsTableInFrom_ isMaybe table froml) <> (getIsTableInFrom_ isMaybe table fromr)
    TABLE table_ -> if table == table_ then [isMaybe] else []

getIsTableInFrom :: Table -> FROM -> Array (IsMaybeTable)
getIsTableInFrom = getIsTableInFrom_ false

data SELECT a = SELECT FROM WHERE ORDER_BY (ColGroup a)

instance showSELECT :: Show (SELECT a) where
  show (SELECT from where_ orderBy colGroup) =
    "SELECT"
      <> show colGroup
      <> "\nFROM"
      <> show from
      <> showWheres 1 where_
      <> showOrders orderBy

where_ :: forall a. Array WhereExpr -> Either String (SELECT a) -> Either String (SELECT a)
where_ wheres =  map (whereW wheres)

whereW :: forall a. Array WhereExpr -> SELECT a -> SELECT a
whereW wheres (SELECT from (WHERE currentWheres) orders colGroup) =
      (SELECT from (WHERE (currentWheres <> wheres)) orders colGroup)

newtype WHERE = WHERE (Array WhereExpr)

data WhereExpr
  = ColEq String SqlValue
  | ColNull String
  | ColEqSubQuery (Int -> String) (Array SqlValue)

class EqColumn col value result where
  eqColumn :: col -> value -> result

instance eqColumnWhereSqlValue :: (IsSqlValue a) => EqColumn (Column a) a WhereExpr where
  eqColumn col val = ColEq (show col) (toSql val)

instance eqColumnWhereNull ::  EqColumn (Column a) Unit WhereExpr where
  eqColumn col unit = ColNull (show col)

instance eqColumnWhereQuery :: EqColumn (Column a) (SELECT Unit) WhereExpr where
  eqColumn col1 fromWCols@(SELECT from wheres orders (ColGroup _ _ _ col2)) =
    ColEqSubQuery
      (\paramCount -> show col1 <> " = (" <> (showSELECTWithParamCount paramCount fromWCols) <> ")")
      (getParams fromWCols)

whereQuery :: forall a. (Show a) => Column a -> (SELECT Unit) -> WhereExpr
whereQuery col1 fromWCols@(SELECT from wheres orders (ColGroup _ _ _ col2)) =
  ColEqSubQuery
    (\paramCount -> show col1 <> " = (" <> (showSELECTWithParamCount paramCount fromWCols) <> ")")
    (getParams fromWCols)

selectW :: forall a. Column a -> ColGroup Unit
selectW c@(Column column) =
    ColGroup
      [{name: column.name, table:column.table}]
      []
      []
      (Column
         { name:column.name
         , table:column.table
         , d: const $ fail $ ForeignError "selectW should not use decoder"
         }
      )

fromWInternal :: forall a. FROM -> ColGroup Unit -> SELECT Unit
fromWInternal from_ colGroup =
  SELECT from_ (WHERE []) (ORDER_BY []) colGroup

fromW_ :: forall a. Table -> ColGroup Unit -> SELECT Unit
fromW_ t = fromWInternal (TABLE t)

fromW :: forall a. Table -> (FROM -> FROM) -> ColGroup Unit -> SELECT Unit
fromW table toFrom =
  fromWInternal (toFrom (TABLE table))

showSELECTWithParamCount :: forall a. Show a => Int -> (SELECT a) -> String
showSELECTWithParamCount paramCount (SELECT from wheres orders colGroup) =
  "SelectInto " <> show from
  <> showWheres paramCount wheres
  <> showOrders orders

showWheres :: Int -> WHERE -> String
showWheres paramCount (WHERE wheres) = if null wheres
  then ""
  else "\nWHERE " <> joinWith "\nAND " (go [] paramCount wheres)
  where
    go :: Array String -> Int -> Array WhereExpr -> Array String
    go result paramCount wheres = case uncons wheres of
      Nothing -> result
      Just {head, tail} -> case head of
        ColEq str _ ->
          go (result <> [str <> " = " <> "$" <> show paramCount]) (paramCount + 1) tail
        ColNull str  ->
          go (result <> [str <> " = NULL"]) paramCount tail
        ColEqSubQuery queryString params ->
          go (result <> [queryString paramCount]) (paramCount + length params) tail

getParams :: forall a. SELECT a -> Array SqlValue
getParams (SELECT _ (WHERE wheres) _ _) = wheres >>= \whereExpr ->
  case whereExpr of
    ColNull _ -> []
    ColEq _ sqlValue -> [sqlValue]
    ColEqSubQuery _ params -> params

orderBy :: forall a. Array Order -> Either String (SELECT a) -> Either String (SELECT a)
orderBy orders eFromWC = map go eFromWC
  where
    go (SELECT from wheres (ORDER_BY currentOrders) colGroup) =
      (SELECT from wheres (ORDER_BY (currentOrders <> orders)) colGroup)

data ORDER_BY = ORDER_BY (Array Order)

data Order = Order String Direction

instance showOrder :: Show Order where
  show (Order str dir) = str <> " " <> (show dir)

data Direction = ASC | DESC

instance showDirection :: Show Direction where
  show ASC = "ASC"
  show DESC = "DESC"

asc :: forall a. Column a -> Order
asc c = Order (show c) ASC

desc :: forall a. Column a -> Order
desc c = Order (show c) DESC

showOrders :: ORDER_BY -> String
showOrders (ORDER_BY orders) = if null orders
  then ""
  else "\nORDER BY\n  " <> joinWith ",\n  " (map show orders)

-- INFIXES

infixl 4 combine as ..
infixl 1 colIntoColGroup as &*
infixl 1 colIntoColGroupM as &?
infixl 0 eqColumn as ===

-- instance selectExprSelectAndColGroup :: SelectExpr (SelectAndColGroup (a -> b)) (ColGroup)
-- nullable ::

-- on :: forall a. Table -> EqC a -> On
-- on s (EqC c1 c2) = On ("ON " <> show c1 <> " = " <> show c2) (TABLE s)

-- data SelectWithFn
