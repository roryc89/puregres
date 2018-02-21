# Puregres

Converts sql into purescript code. To run tests, create a .env file in the root and add your db username (and password if necessary) eg.
`DB_USER=my_user_name`

Run the tests and look in the test/Queries directory to see how it works:
`npm t`

## Example

This sql file called `Query/Select/SelectWithSubQuery.sql`
```sql
SELECT order_id,
       user_id
FROM orders
WHERE user_id =
    (SELECT user_id
     FROM users
     WHERE email = $1)
```

Will be converted to a purescript file like this:
```purs
module Query.Select.SelectWithSubQuery (selectWithSubQuery, SelectWithSubQueryRow) where

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
```
