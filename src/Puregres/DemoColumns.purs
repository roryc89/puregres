module Puregres.DemoColumns where

import Control.Apply (apply, lift2)
import Control.Monad.Free (resume)
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (decode)
import Data.Foreign.Index ((!))
import Data.Function as Function
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.Postgres.SqlValue (SqlValue)
import Prelude (class Functor, class Show, Unit, flip, map, show, unit, (#), ($), (<$>), (<*>), (<>), (>>=))
import Puregres.Class (params)
import Puregres.PuregresSqlValue (class IsSqlValue, decode_, toSql)
import Puregres.Where (WHERE(..), WhereExpr(..), showWheres)
import Puregres.Experiment

data Submissions = Submissions

instance showSubmissions :: Show Submissions where show _ = "submissions"

data Users = Users

instance showUsers :: Show Users where show _ = "users"

data Orders = Orders

instance showOrders :: Show Orders where show _ = "orders"

data Items = Items

instance showItems :: Show Items where show _ = "items"

data ORDER_ID = ORDER_ID

instance showORDER_ID :: Show ORDER_ID where show _ = "public.orders.order_id"

instance columnORDER_ID :: Column ORDER_ID Int

instance colOfTABLE_ORDER_ID :: (ColOf ORDER_ID b Int) => ColOf ORDER_ID (TABLE a b) Int where
  from_ ORDER_ID (TABLE a b) = from_ ORDER_ID b

instance colOfINNER_JOIN_ORDER_ID :: (ColOf ORDER_ID a Int) => ColOf ORDER_ID (INNER_JOIN a) Int where
  from_ ORDER_ID (INNER_JOIN (On _ a)) = from_ ORDER_ID a

instance colOfOrdersORDER_ID :: ColOf ORDER_ID (TABLE Orders a) Int where
  from_ t _ = \f -> f ! (show t) >>= decode_


data ORDER_ITEM_ID = ORDER_ITEM_ID

instance showORDER_ITEM_ID :: Show ORDER_ITEM_ID where show _ = "public.orders.item_id"

instance columnORDER_ITEM_ID :: Column ORDER_ITEM_ID Int

instance colOfTABLE_ORDER_ITEM_ID :: (ColOf ORDER_ITEM_ID b Int) => ColOf ORDER_ITEM_ID (TABLE a b) Int where
  from_ ORDER_ITEM_ID (TABLE a b) = from_ ORDER_ITEM_ID b

instance colOfOrdersORDER_ITEM_ID :: ColOf ORDER_ITEM_ID (TABLE Orders a) Int where
  from_ t _ = \f -> f ! (show t) >>= decode_

instance colOfINNER_JOIN_ORDER_ITEM_ID :: (ColOf ORDER_ITEM_ID a Int) => ColOf ORDER_ITEM_ID (INNER_JOIN a) Int where
  from_ ORDER_ITEM_ID (INNER_JOIN (On _ a)) = from_ ORDER_ITEM_ID a

data ORDER_NAME = ORDER_NAME

instance showORDER_NAME :: Show ORDER_NAME where show _ = "public.orders.order_name"

instance columnORDER_NAME :: Column ORDER_NAME String

instance colOfTABLE_ORDER_NAME :: (ColOf ORDER_NAME b String) => ColOf ORDER_NAME (TABLE a b) String where
  from_ ORDER_NAME (TABLE a b) = from_ ORDER_NAME b

instance colOfOrdersORDER_NAME :: ColOf ORDER_NAME (TABLE Orders a) String where
  from_ t _ = \f -> f ! (show t) >>= decode_

instance colOfINNER_JOIN_ORDER_NAME :: (ColOf ORDER_NAME a String) => ColOf ORDER_NAME (INNER_JOIN a) String where
  from_ ORDER_NAME (INNER_JOIN (On _ a)) = from_ ORDER_NAME a

data ITEM_ID = ITEM_ID

instance showITEM_ID :: Show ITEM_ID where show _ = "public.items.item_id"

instance columnITEM_ID :: Column ITEM_ID Int

instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where
  from_ ITEM_ID (TABLE a b) = from_ ITEM_ID b

instance colOfItemsITEM_ID :: ColOf ITEM_ID (TABLE Items a) Int where
  from_ t _ = \f -> f ! (show t) >>= decode_

instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where
  from_ ITEM_ID (INNER_JOIN (On _ a)) = from_ ITEM_ID a

submissions = TABLE Submissions
items = TABLE Items
orders = TABLE Orders

res2 :: SelectQuery
  { order_id :: Int
  , order_name :: String
  }
res2 = select $ (cols_ f)
  {order_id:_, order_name:_} >>>
  ORDER_ID >> ORDER_NAME
    where
      f = (
        from $ orders $
        inner_join items (on ORDER_ITEM_ID ITEM_ID) unit
        )
        # whereVal ITEM_ID 8
        # whereSub ITEM_ID ORDER_ID
          ((from $ orders unit) # whereVal ORDER_ID 9)
