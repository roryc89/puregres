module Puregres.Where (WHERE(..), WhereExpr(..), showWheres) where

import Prelude

import Data.Array (uncons)
import Data.Foldable (null, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class (class Params)

newtype WHERE = WHERE (Array WhereExpr)

instance semigroup :: Semigroup WHERE where
  append (WHERE wheres1) (WHERE wheres2) = WHERE (wheres1 <> wheres2)

instance monoidWHERE :: Monoid WHERE where
  mempty = WHERE []

data WhereExpr
  = ColEq String SqlValue
  | ColNull String
  | ColEqSubQuery (Int -> String) (Array SqlValue)

instance showWHERE :: Show WHERE where
  show = showWheres 1
  
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

instance paramsWHERE :: Params WHERE where
  params (WHERE wheres) = wheres >>= \whereExpr ->
    case whereExpr of
      ColNull _ -> []
      ColEq _ sqlValue -> [sqlValue]
      ColEqSubQuery _ params -> params
