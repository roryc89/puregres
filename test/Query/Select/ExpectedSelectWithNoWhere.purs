module Query.Select.SelectWithNoWhere (selectWithNoWhere, SelectWithNoWhereRow) where

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

type SelectWithNoWhereRow =
  { email :: String
  , last_name :: Maybe (String)
  , order_id :: Int
  , item_id :: Maybe (Int)
  }

selectWithNoWhere :: forall eff.
  
  Aff (db :: DB | eff) (Array SelectWithNoWhereRow)
selectWithNoWhere  =
  query query_ []
  # map (map toRow)
  where
    toRow :: Foreign -> SelectWithNoWhereRow
    toRow f =
      { email: unsafeRemoveFromFail $ f ! "email" >>= decode
      , last_name: unsafeRemoveFromFail $ f ! "last_name" >>= readNull >>= traverse decode
      , order_id: unsafeRemoveFromFail $ f ! "order_id" >>= decode
      , item_id: unsafeRemoveFromFail $ f ! "item_id" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT email,
       last_name,
       order_id,
       item_id
FROM users
INNER JOIN orders
  ON users.user_id = orders.user_id

"""
