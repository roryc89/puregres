module Puregres.Insert where

import Prelude
import Data.Array (mapWithIndex, length)
import Data.Foldable (foldr, foldl)
import Data.String (joinWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Foreign (Foreign, F)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Database.Postgres.SqlValue (SqlValue)
import Puregres.Class
import Puregres.PuregresSqlValue (class IsSqlValue, toSql)
import Puregres.Where (WHERE(..))

data InsertCols t = InsertCols (Array String) (Array SqlValue)

instance showInsertCols :: Show (InsertCols t) where
  show (InsertCols cols _) =
    "\n  (" <> joinWith ", " cols <> ")"
    <> "\nVALUES"
    <> "\n  ("
    <> (joinWith ", " $ mapWithIndex (\i _ -> "$" <> show (i + 1)) cols)
    <> ")\n"

instance paramsInsertCols :: Params (InsertCols t) where
  params (InsertCols _ p) = p

col :: forall t c r. ColOf c t r => IsSqlValue r => c /\ r -> InsertCols t
col (c /\ r) = InsertCols [colName c ] [toSql r]

andCol :: forall t c r. ColOf c t r => Show c => IsSqlValue r =>
  InsertCols t
  -> c /\ r
  -> InsertCols t
andCol (InsertCols cs sqls) (c /\ r) = InsertCols newCs newSqls
  where
    newCs = cs <> [colName c]
    newSqls = sqls <> [toSql r]

infixl 3 andCol as ++

data InsertInto t c = InsertInto t (InsertCols t) (Maybe c)

insertInto :: forall t c r. ColOf c t r => t -> InsertCols t -> InsertInto t c
insertInto table into =
  InsertInto table into Nothing

insertIntoReturning :: forall t c1 c2 r. ColOf c1 t r => ColOf c2 t r =>
  t
  -> InsertCols t
  -> Maybe c1
  -> InsertInto t c1
insertIntoReturning table into returning =
  InsertInto table into returning

instance showInsertInto :: (Show t, Show c) => Show (InsertInto t c) where
  show (InsertInto t cols returningM) =
    "INSERT INTO"
    <> show t
    <> show cols
    <> returning
    where
      returning = maybe "" (\c -> "\nRETURNING " <> show c) returningM
      
instance paramsInsertInto :: Params (InsertInto t c) where
  params (InsertInto _ insertCols _) = params insertCols

-- data InsertInto t = InsertInto (Array String) (Array )
-- insertInto ::
--
-- data SizedMatrix i a = Array (Vec i a)
--
--
-- insertInto :: forall c t r ColOf c t r => t -> InsertCols t
-- data INSERT = INSERT Table ColsAndVals WHERE

-- val :: forall a. (IsSqlValue a) => Column a -> a -> ColAndVal
-- val column value = ColAndVal (removeDecoder column) (toSql value)
--
-- insertInto :: Table -> Array ColAndVal -> INSERT
-- insertInto table cols = INSERT table (ColsAndVals cols) (WHERE [])
--
-- instance showINSERT :: Show INSERT where
--   show (INSERT table colAndVals (WHERE wheres)) =
--     "INSERT INTO "
--       <> show table
--       <> "\n"
--       <> show colAndVals
--
-- data ColAndVal = ColAndVal ColumnWoDecoder SqlValue
--
-- data ColsAndVals = ColsAndVals (Array ColAndVal)
--
--
-- instance paramsINSERT :: Params INSERT where
--   params (INSERT _  colsAndVals where_) = params colsAndVals <> params where_
--
-- instance showColsAndVals :: Show ColsAndVals where
--   show (ColsAndVals values) =
--     "(" <> (joinWith ", " $ showCol <$> values) <> ")"
--     <> "\nVALUES\n"
--     <>  "(" <> (joinWith ", " $ mapWithIndex showValue values) <> ")"
--     where
--       showCol (ColAndVal (ColumnWoDecoder {name}) _) = name
--       showValue i _ = "$" <> show i
--
-- instance paramsColsAndVals :: Params ColsAndVals where
--   params (ColsAndVals colsAndVals) = map (\(ColAndVal _ val) -> val) colsAndVals
