module Query.Users.SelectUsersAndOrders (selectUsersAndOrders, SelectUsersAndOrdersRow) where

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
import Schema.UnsafeRemoveFromFail (remove)

type SelectUsersAndOrdersRow =
  { email :: String
  , order_id :: Int
  , item_id :: Maybe (Int)
  }

selectUsersAndOrders :: forall eff.
  Int
  -> Int
  -> Aff (db :: DB | eff) (Array SelectUsersAndOrdersRow)
selectUsersAndOrders item_id user_id =
  query query_ [toSql item_id, toSql user_id]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectUsersAndOrdersRow
    toRow f =
      { email: remove $ f ! "email" >>= decode
      , order_id: remove $ f ! "order_id" >>= decode
      , item_id: remove $ f ! "item_id" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT email,
       order_id,
       item_id
FROM users
INNER JOIN orders
  ON users.user_id = orders.user_id
WHERE users.user_id = $2
  AND item_id = $1

"""
