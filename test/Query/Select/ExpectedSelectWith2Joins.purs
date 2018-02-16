module Query.Select.SelectWith2Joins (selectWith2Joins, SelectWith2JoinsRow) where

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

type SelectWith2JoinsRow =
  { user_id :: Int
  , email :: String
  , registered :: Boolean
  , order_id :: Int
  }

selectWith2Joins :: forall eff.
  Int
  -> Aff (db :: DB | eff) (Array SelectWith2JoinsRow)
selectWith2Joins item_id =
  query query_ [toSql item_id]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectWith2JoinsRow
    toRow f =
      { user_id: unsafeRemoveFromFail $ f ! "user_id" >>= decode
      , email: unsafeRemoveFromFail $ f ! "email" >>= decode
      , registered: unsafeRemoveFromFail $ f ! "registered" >>= decode
      , order_id: unsafeRemoveFromFail $ f ! "order_id" >>= decode
      }

query_ :: String
query_ = """
SELECT users.user_id,
       email,
       registered,
       order_id
FROM items
INNER JOIN orders ON items.item_id = orders.item_id
INNER JOIN users ON orders.user_id = users.user_id
WHERE items.item_id = $1

"""
