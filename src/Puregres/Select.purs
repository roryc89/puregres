module Puregres.Select where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array (length, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (sequence)
import Database.Postgres (Client, DB, Query(..), query)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Database.Postgres.SqlValue (SqlValue)
import ExpectedTables.Items as Items
import Puregres.Type (Column(Column), Table(Table), addMaybe, andCol, makeColumn)
import Unsafe.Coerce (unsafeCoerce)

data SELECT a = SELECT (From a) (Array WhereExpr) (Array Order)

select :: forall a. Either String (From a) -> Either String (SELECT a)
select eFrom = map (\from -> SELECT from [] []) eFrom

instance showSELECT :: Show a => Show (SELECT a) where
  show (SELECT from wheres order) =
    "SELECT " <> show from
    <> showWheres wheres

data FromExpr
  = LEFT_JOIN FromExpr On
  | INNER_JOIN FromExpr On
  | TABLE Table

instance showFromExpr :: Show FromExpr where
  show (LEFT_JOIN sel on) = show sel <> "\n  LEFT JOIN" <> show on
  show (INNER_JOIN sel on) = show sel <> "\n  INNER JOIN" <> show on
  show (TABLE t) = " " <> (show t)

data On = On String FromExpr

instance onShow :: Show On where
  show (On str sel) = show sel <> " " <> str

on :: forall a. Table -> EqC a -> On
on s (EqC c1 c2) = On ("ON " <> show c1 <> " = " <> show c2) (TABLE s)

data EqC a = EqC (Column a) (Column a)

eqC :: forall a. Column a -> Column a -> EqC a
eqC = EqC

data WhereExpr
  = ColEq String SqlValue
  | ColNull String

where_ :: forall a. Either String (SELECT a) -> Array WhereExpr -> Either String (SELECT a)
where_ eSel wheres = map (\(SELECT from existingWheres orders) ->
  SELECT from (existingWheres <> wheres) orders) eSel

is :: forall a. (IsSqlValue a) => Column a -> a -> WhereExpr
is c v = ColEq (show c) (toSql v)

isNull :: forall a. Column a -> WhereExpr
isNull c = ColNull (show c)

showWheres :: Array WhereExpr -> String
showWheres wheres = if null wheres
  then ""
  else "\nWHERE " <> joinWith "\nAND " (mapWithIndex showWhere wheres)
  where
    showWhere index where_ = case where_ of
      (ColEq str _) -> str <> " = " <> "$" <> show (index + 1)
      (ColNull str ) -> str <> " = " <> "NULL"

data Order = Order String Direction

data Direction = ASC | DESC

instance showDirection :: Show Direction where
  show ASC = "ASC"
  show DESC = "DESC"

reasonColNotValidInFrom :: forall a. Boolean -> Column a -> FromExpr -> Maybe String -- string is reason for not being valid
reasonColNotValidInFrom isMaybeColumn col sel = case (go false col sel) of
    [isMaybe] -> if isMaybeColumn == isMaybe
      then Nothing
      else Just $ if isMaybeColumn
        then "Searching for a maybe column but non maybe column found. Try adding ? to the column combinator. Column: " <> show col
        else "Searching for a non maybe column but maybe column found. Try removing ? from the column combinator. Column: " <> show col

    [] -> Just $ "No tables with column found: " <> show col
    _ -> Just $ "Multiple tables with column found: " <> show col
  where
    go :: Boolean -> Column a -> FromExpr -> Array (Boolean)
    go isMaybe col@(Column c) sel =
      case sel of
        LEFT_JOIN sell (On _ selr) -> (go isMaybe col sell) <> (go true col selr)
        INNER_JOIN sell (On _ selr) -> (go isMaybe col sell) <> (go isMaybe col selr)
        TABLE table -> if (table == c.table) then [isMaybe] else []

colIntoFrom_ :: forall a b. Boolean -> Column a -> From (a -> b) -> Either String (From (Column b))
colIntoFrom_ isMaybe colv (From f sel) =
  case reasonColNotValidInFrom isMaybe colv sel of
    Nothing -> Right $ From (f <$> colv) sel
    Just reason -> Left reason

colIntoFrom :: forall a b. Column a -> From (a -> b) -> Either String (From (Column b))
colIntoFrom = colIntoFrom_ false

maybeColIntoFrom :: forall b a. Decode a => Column a -> From (Maybe a -> b) -> Either String (From (Column b))
maybeColIntoFrom col = colIntoFrom_ true (addMaybe col)

anotherColIntoFrom_ :: forall a b.
  Boolean
  -> Column a
  -> Either String (From (Column (a -> b)))
  -> Either String (From (Column b))
anotherColIntoFrom_ isMaybe colv eFrom = bind eFrom \(From f sel) ->
  case reasonColNotValidInFrom isMaybe colv sel of
    Nothing -> Right $ From (andCol f colv) sel
    Just reason -> Left reason

anotherColIntoFrom :: forall a b.
  Column a
  -> Either String (From (Column (a -> b)))
  -> Either String (From (Column b))
anotherColIntoFrom = anotherColIntoFrom_ false

maybeAnotherColIntoFrom :: forall a b. Decode a =>
  Column a
  -> Either String (From (Column (Maybe a -> b)))
  -> Either String (From (Column b))
maybeAnotherColIntoFrom col = anotherColIntoFrom_ true (addMaybe col)

data From a = From a FromExpr

instance showFrom :: Show a => Show (From a) where
  show (From c expr ) = show c <> "\nFROM" <> show expr

fromF :: forall a. FromExpr -> a -> From a
fromF = flip From

colIntoFromFlipped :: forall a b.
  From (a -> b)
  -> Column a
  -> Either String (From (Column b))
colIntoFromFlipped = flip colIntoFrom

maybeColIntoFromFlipped :: forall a b. Decode a =>
  From (Maybe a -> b)
  -> Column a
  -> Either String (From (Column b))
maybeColIntoFromFlipped = flip maybeColIntoFrom

anotherColIntoFromFlipped :: forall a b.
  Either String (From (Column (a -> b)))
  -> Column a
  -> Either String (From (Column b))
anotherColIntoFromFlipped = flip anotherColIntoFrom

maybeAnotherColIntoFromFlipped :: forall a b. Decode a =>
  Either String (From (Column (Maybe a -> b)))
  -> Column a
  -> Either String (From (Column b))
maybeAnotherColIntoFromFlipped = flip maybeAnotherColIntoFrom

infixl 4 colIntoFrom as <**
infixr 4 colIntoFromFlipped as **>
infixl 4 maybeColIntoFrom as <??
infixl 4 maybeColIntoFromFlipped as ??>
infixl 3 anotherColIntoFrom as &*
infixl 3 anotherColIntoFromFlipped as *&
infixl 3 maybeAnotherColIntoFrom as &?
infixl 3 maybeAnotherColIntoFromFlipped as ?&

fakeClient :: Client
fakeClient = unsafeCoerce unit

fromExpr :: FromExpr
fromExpr =
  TABLE Items.items
    `INNER_JOIN` (orders `on` (Items.item_id `eqC` item_id))

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
