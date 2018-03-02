module Puregres.Select where

import Prelude

import Data.Array (length, mapWithIndex, uncons)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Foreign.Class (class Decode)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (sequence)
import Database.Postgres (Client, DB, Query(..), query)
import Database.Postgres.SqlValue (SqlValue)
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Type (Column(Column), Table, addMaybe, andCol)

data SELECT a = SELECT (From a) (Array WhereExpr) (Array Order)

select :: forall a. Either String (From a) -> Either String (SELECT a)
select eFrom = map (\from -> SELECT from [] []) eFrom

instance showSELECT :: Show a => Show (SELECT a) where
  show = showSelectWParamCount 1

showSelectWParamCount :: forall a. Show a => Int -> SELECT a -> String
showSelectWParamCount paramCount (SELECT from wheres orders) =
  "SELECT " <> show from
  <> showWheres paramCount wheres
  <> showOrders orders

getParams :: forall a. SELECT a -> Array SqlValue
getParams (SELECT _ wheres _) = wheres >>= \whereExpr ->
  case whereExpr of
    ColNull _ -> []
    ColEq _ sqlValue -> [sqlValue]
    ColEqSubQuery _ params -> params

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
  | ColEqSubQuery (Int -> String) (Array SqlValue)

where_ :: forall a. Array WhereExpr -> Either String (SELECT a) -> Either String (SELECT a)
where_ wheres eSel = map (\(SELECT from existingWheres orders) ->
  SELECT from (existingWheres <> wheres) orders) eSel

is :: forall a. (IsSqlValue a) => Column a -> a -> WhereExpr
is c v = ColEq (show c) (toSql v)

isNull :: forall a. Column a -> WhereExpr
isNull c = ColNull (show c)

isQuery :: forall a . Column a -> Column a -> FromExpr -> Array WhereExpr -> WhereExpr
isQuery col1 col2 from wheres =
  ColEqSubQuery
    (\paramCount -> show col1 <> " = (" <> (showSelectWParamCount paramCount sel) <> ")")
    (getParams sel)
  where
    sel = SELECT (From col2 from) wheres []


showWheres :: Int -> Array WhereExpr -> String
showWheres paramCount wheres = if null wheres
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

        -- _ -> go result paramCount tail -- TODO: complete for sub queries

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

orderBy :: forall a. Array Order -> Either String (SELECT a) -> Either String (SELECT a)
orderBy orders eSel = map (\(SELECT from wheres existingOrders) ->
  SELECT from wheres (orders <> existingOrders)) eSel

showOrders :: Array Order -> String
showOrders orders = if null orders
  then ""
  else "\nORDER BY\n  " <> joinWith ",\n  " (map show orders)

data From a = From a FromExpr

instance showFrom :: Show a => Show (From a) where
  show (From c expr ) = show c <> "\nFROM" <> show expr

fromF :: forall a. FromExpr -> a -> From a
fromF = flip From

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
