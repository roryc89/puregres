module Query.Select.SelectUsersWhereId (selectUsersWhereId, SelectUsersWhereIdRow) where

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

type SelectUsersWhereIdRow =
  { user_id :: Int
  , email :: String
  , last_name :: Maybe (String)
  }

selectUsersWhereId :: forall eff.
  Int
  -> Aff (db :: DB | eff) (Array SelectUsersWhereIdRow)
selectUsersWhereId user_id =
  query query_ [toSql user_id]
  # map (map toRow)
  where
    toRow :: Foreign -> SelectUsersWhereIdRow
    toRow f =
      { user_id: unsafeRemoveFromFail $ f ! "user_id" >>= decode
      , email: unsafeRemoveFromFail $ f ! "email" >>= decode
      , last_name: unsafeRemoveFromFail $ f ! "last_name" >>= readNull >>= traverse decode
      }

query_ :: String
query_ = """
SELECT user_id,
       users.email,
       public.users.last_name
FROM users
WHERE users.user_id = $1
  AND registered = TRUE

"""
