module Puregres.Where (WHERE(..), WhereExpr(..), showWheres) where

import Prelude

import Data.Array (uncons)
import Data.Foldable (null, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class (class Params)
import Puregres.Comparator (Comparator)
import Puregres.PuregresSqlValue (class IsSqlValue)
import Unsafe.Coerce (unsafeCoerce)

newtype WHERE = WHERE (Array WhereExpr)

instance semigroup :: Semigroup WHERE where
  append (WHERE wheres1) (WHERE wheres2) = WHERE (wheres1 <> wheres2)

instance monoidWHERE :: Monoid WHERE where
  mempty = WHERE []

data WhereExpr
  = ColVal String Comparator SqlValue
  | ColAny String Comparator String (Array SqlValue)
  | ColNull String
  | ColValSubQuery (Int -> String) (Array SqlValue)

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
        ColVal str comparator _ ->
          go (result <> [str <> show comparator <> "$" <> show paramCount]) (paramCount + 1) tail
        ColAny str comparator postType vals ->
          go
            (result <> [str <> show comparator <> "ANY($" <> show paramCount <> "::" <> postType <> ")"])
            (paramCount + 1)
            tail
        ColNull str  ->
          go (result <> [str <> " = NULL"]) paramCount tail
        ColValSubQuery queryString params ->
          go (result <> [queryString paramCount]) (paramCount + length params) tail

instance paramsWHERE :: Params WHERE where
  params (WHERE wheres) = wheres >>= \whereExpr ->
    case whereExpr of
      ColNull _ -> []
      ColVal _ _ sqlValue -> [sqlValue]
      ColAny _ _ _ sqlValue -> [toSqlCoerce sqlValue]
      ColValSubQuery _ params -> params

toSqlCoerce :: forall a. (IsSqlValue a) => Array a -> SqlValue
toSqlCoerce = unsafeCoerce
