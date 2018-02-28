module Puregres.Select where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Database.Postgres (Client, DB, Query(..), query)
import Database.Postgres.SqlValue (class IsSqlValue, SqlValue, toSql)
import ExpectedTables.Items as Items
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Puregres.Type (Column(Column), Table(Table), addMaybe, makeColumn, (&*))
import Unsafe.Coerce (unsafeCoerce)

data SelectExpr
  = LeftJoin SelectExpr On
  | InnerJoin SelectExpr On
  | TableExpr Table

instance selectExprShow :: Show SelectExpr where
  show (LeftJoin sel on) = " \n " <> show sel <> " LEFT JOIN " <> " " <> show on
  show (InnerJoin sel on) = " \n " <> show sel <> " INNER JOIN " <> " " <> show on
  show (TableExpr (Table s)) = " " <> s <> " "

data On = On String SelectExpr

instance onShow :: Show On where
  show (On str sel) = show sel <> " " <> str

on :: forall a. Table -> EqC a -> On
on s (EqC (Column c1) (Column c2)) = On ("\n On " <> c1.serialized <> " = " <> c2.serialized) (TableExpr s)

leftJoin :: SelectExpr -> On -> SelectExpr
leftJoin = LeftJoin

data EqC a = EqC (Column a) (Column a)

eqC :: forall a. Column a -> Column a -> EqC a
eqC = EqC

is :: forall a. (IsSqlValue a) => Column a -> a -> WhereExpr
is (Column c) v = ColEq c.serialized (toSql v)

whereNull :: forall a. Column a -> WhereExpr
whereNull (Column c) = ColNull c.serialized

data WhereExpr
  = ColEq String SqlValue
  | ColNull String
  | ColSub SelectExpr

data ColumnAndSelect a = ColumnAndSelect SelectExpr (Column a)

instance functorColumnAndSelect :: Functor ColumnAndSelect where
  map f (ColumnAndSelect s col) = ColumnAndSelect s (map f col)

instance columnApply :: Apply ColumnAndSelect where
  apply (ColumnAndSelect s col1) (ColumnAndSelect _ col2) =
    ColumnAndSelect s $ apply col1 col2

colIntoSelect :: forall a. Boolean -> Column a -> SelectExpr -> Either String (ColumnAndSelect a)
colIntoSelect isMaybeColumn c sel = case (go false c sel) of
    [isMaybe] -> if isMaybeColumn == isMaybe
      then Right $ ColumnAndSelect sel c
      else Left $ if isMaybeColumn
        then "Searching for a maybe column but non maybe column found. Try adding ?? to the column. Column: " <> show c
        else "Searching for a non maybe column but maybe column found. Try removing ?? from the column. Column: " <> show c

    [] -> Left $ "No tables with column found: " <> show c
    _ -> Left $ "Multiple tables with column found: " <> show c
  where
    go :: Boolean -> Column a -> SelectExpr -> Array (Boolean)
    go isMaybe col@(Column c) sel =
      case sel of
        LeftJoin sell (On _ selr) -> (go isMaybe col sell) <> (go true col selr)
        InnerJoin sell (On _ selr) -> (go isMaybe col sell) <> (go isMaybe col selr)
        TableExpr table -> if (table == c.table) then [isMaybe] else []

reasonColNotValidInSelect :: forall a. Boolean -> Column a -> SelectExpr -> Maybe String -- string is reason for not being valid
reasonColNotValidInSelect isMaybeColumn col sel = case (go false col sel) of
    [isMaybe] -> if isMaybeColumn == isMaybe
      then Nothing
      else Just $ if isMaybeColumn
        then "Searching for a maybe column but non maybe column found. Try adding ?? to the column. Column: " <> show col
        else "Searching for a non maybe column but maybe column found. Try removing ?? from the column. Column: " <> show col

    [] -> Just $ "No tables with column found: " <> show col
    _ -> Just $ "Multiple tables with column found: " <> show col
  where
    go :: Boolean -> Column a -> SelectExpr -> Array (Boolean)
    go isMaybe col@(Column c) sel =
      case sel of
        LeftJoin sell (On _ selr) -> (go isMaybe col sell) <> (go true col selr)
        InnerJoin sell (On _ selr) -> (go isMaybe col sell) <> (go isMaybe col selr)
        TableExpr table -> if (table == c.table) then [isMaybe] else []

colIntoFrom_ :: forall a b. Boolean -> Column a -> From (a -> b) -> Either String (From (Column b))
colIntoFrom_ isMaybe colv (From f sel) =
  case reasonColNotValidInSelect isMaybe colv sel of
    Nothing -> Right $ From (f <$> colv) sel
    Just reason -> Left reason

colIntoFrom :: forall a b. Column a -> From (a -> b) -> Either String (From (Column b))
colIntoFrom = colIntoFrom_ false

maybeColIntoFrom :: forall b a. Decode a => Column a -> From (Maybe a -> b) -> Either String (From (Column b))
maybeColIntoFrom col = colIntoFrom_ true (addMaybe col)

anotherColIntoFrom_ :: forall a b. Boolean -> Column a -> Either String (From (Column (a -> b))) -> Either String (From (Column b))
anotherColIntoFrom_ isMaybe colv eFrom = bind eFrom \(From f sel) ->
  case reasonColNotValidInSelect isMaybe colv sel of
    Nothing -> Right $ From (f <*> colv) sel
    Just reason -> Left reason

anotherColIntoFrom :: forall a b. Column a -> Either String (From (Column (a -> b))) -> Either String (From (Column b))
anotherColIntoFrom = anotherColIntoFrom_ false

maybeAnotherColIntoFrom :: forall a b. Decode a => Column a -> Either String (From (Column (Maybe a -> b))) -> Either String (From (Column b))
maybeAnotherColIntoFrom col = anotherColIntoFrom_ true (addMaybe col)

data From a = From a SelectExpr

infixr 4 colIntoFrom as <<
infixl 4 maybeColIntoFrom as <<?
infixr 3 anotherColIntoFrom as &
infixl 3 maybeAnotherColIntoFrom as &?


colIntoSelectUnsafe :: forall a. Column a -> SelectExpr -> (ColumnAndSelect a)
colIntoSelectUnsafe c sel = colIntoSelect false c sel # \ec ->
  case ec of
    Left msg -> unsafePartial $ crash $ "Error. Could not build query: " <> (show msg)
    Right c -> c

colIntoSelectCol :: forall a b.
  Boolean
  -> Column (a -> b)
  -> Either String (ColumnAndSelect a)
  -> Either String (ColumnAndSelect b)
colIntoSelectCol isMaybe col1 eColSel = go (colIntoSelect isMaybe col1 selectExpr) eColSel
  where
    go :: Either String (ColumnAndSelect (a -> b)) -> Either String (ColumnAndSelect a) -> Either String (ColumnAndSelect b)
    go = lift2 apply

andCol :: forall a b. Column (a -> b) -> Either String (ColumnAndSelect a) -> Either String (ColumnAndSelect b)
andCol = colIntoSelectCol false

colIntoSelectColUnsafe :: forall a b.
  Boolean
  -> Column (a -> b)
  -> ColumnAndSelect a
  -> ColumnAndSelect b
colIntoSelectColUnsafe isMaybe col1 colSel@(ColumnAndSelect selectExpr col2) =
  (colIntoSelectUnsafe col1 selectExpr) <*> colSel


fakeClient :: Client
fakeClient = unsafeCoerce unit

select :: forall a. Column a -> SelectExpr -> Array WhereExpr -> Aff (db :: DB) (F (Array a))
select col@(Column c) sel whereExprs = do
  result :: Array Foreign <- query (Query (show col <> " FROM " <> show sel)) [] fakeClient
  pure $ sequence $ map c.d result

selectExpr :: SelectExpr
selectExpr =
  TableExpr Items.items
    `InnerJoin` (orders `on` (Items.item_id `eqC` item_id))

getOrdersByItemId :: Int -> Aff( db :: DB) (F (Array {order_id :: Int, user_id :: Int}))
getOrdersByItemId itemId = select
  ({order_id:_, user_id:_} <$> order_id &* user_id)
  (TableExpr Items.items
    `InnerJoin` (orders `on` (Items.item_id `eqC` item_id)))
  [ whereNull order_notes
  , is item_id itemId
  ]

orders :: Table
orders = Table "public.orders"

order_id :: Column Int
order_id = makeColumn orders "order_id"

user_id :: Column Int
user_id = makeColumn orders "user_id"

item_id :: Column Int
item_id = makeColumn orders "item_id"

order_notes :: Column (NullOrUndefined String)
order_notes = makeColumn orders "order_notes"
