module Query.Select.SelectWithOuterJoin (selectWithOuterJoin, SelectWithOuterJoinRow) where

import Prelude
import DB (query)
import Control.Monad.Aff (Aff)
import Data.Foreign (Foreign, readNull)
import Data.Foreign.Class (decode)
import Data.Foreign.Index ((!))
import Database.Postgres.SqlValue (toSql)
import Data.Maybe (Maybe)
import Database.Postgres (DB)
import Data.Traversable (traverse)
import Puregres.UnsafeRemoveFromFail (unsafeRemoveFromFail)

type SelectWithOuterJoinRow =
  { email :: Maybe (String)
  , order_id :: Maybe (Int)
  , item_id :: Maybe (Int)
  }

selectWithOuterJoin :: forall eff.
  Int
  -> Aff (db :: DB | eff) (Array SelectWithOuterJoinRow)
selectWithOuterJoin item_id =
  query query_ [toSql item_id]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectWithOuterJoinRow
    toRow f =
      { email: unsafeRemoveFromFail $ f ! "email" >>= readNull >>= traverse decode
      , order_id: unsafeRemoveFromFail $ f ! "order_id" >>= readNull >>= traverse decode
      , item_id: unsafeRemoveFromFail $ f ! "item_id" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT email,
       order_id,
       item_id
FROM users
FULL OUTER JOIN orders
  ON users.user_id = orders.user_id
WHERE orders.item_id = $1

"""
