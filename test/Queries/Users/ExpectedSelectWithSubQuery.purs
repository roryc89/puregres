module Query.Users.SelectWithSubQuery (selectWithSubQuery, SelectWithSubQueryRow) where

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

type SelectWithSubQueryRow =
  { order_id :: Int
  , user_id :: Maybe (Int)
  }

selectWithSubQuery :: forall eff.
  String
  -> Aff (db :: DB | eff) (Array SelectWithSubQueryRow)
selectWithSubQuery email =
  query query_ [toSql email]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectWithSubQueryRow
    toRow f =
      { order_id: unsafeRemoveFromFail $ f ! "order_id" >>= decode
      , user_id: unsafeRemoveFromFail $ f ! "user_id" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT order_id,
       user_id
FROM orders
WHERE user_id =
    (SELECT user_id
     FROM users
     WHERE email = $1)

"""
