module Query.Select.SelectWithLeftJoin (selectWithLeftJoin, SelectWithLeftJoinRow) where

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

type SelectWithLeftJoinRow =
  { email :: String
  , order_id :: Maybe (Int)
  , item_id :: Maybe (Int)
  }

selectWithLeftJoin :: forall eff.
  Int
  -> Aff (db :: DB | eff) (Array SelectWithLeftJoinRow)
selectWithLeftJoin user_id =
  query query_ [toSql user_id]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectWithLeftJoinRow
    toRow f =
      { email: unsafeRemoveFromFail $ f ! "email" >>= decode
      , order_id: unsafeRemoveFromFail $ f ! "order_id" >>= readNull >>= traverse decode
      , item_id: unsafeRemoveFromFail $ f ! "item_id" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT email,
       order_id,
       item_id
FROM users
LEFT JOIN orders
  ON users.user_id = orders.user_id
WHERE users.user_id = $1

"""
