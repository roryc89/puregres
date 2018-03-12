module Test.Puregres.TestData where

import Data.Foreign.Index ((!))
import Data.Foreign (F, Foreign)
import Data.Maybe (Maybe)
import Prelude
import Puregres.PuregresSqlValue (decode_, class IsSqlValue)
import Puregres.Select

data Orders = Orders
instance showOrders :: Show Orders where show _ = "orders"

orders :: forall a. a -> TABLE Orders a
orders = TABLE Orders

data ORDER_ID = ORDER_ID
instance columnORDER_ID :: Column ORDER_ID Int
instance showORDER_ID :: Show ORDER_ID where show _ = "orders.order_id"
instance colOfTABLE_ORDER_ID :: (ColOf ORDER_ID b Int) => ColOf ORDER_ID (TABLE a b) Int where from_ = fromTable
instance colOfINNER_JOIN_ORDER_ID :: (ColOf ORDER_ID a Int) => ColOf ORDER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
instance colOfAAAOrdersORDER_ID :: ColOf ORDER_ID (TABLE Orders a) Int where from_ = getPropAndDecode
-- item_id :: Column Int
-- item_id = makeColumn orders "item_id"
data ITEM_ID = ITEM_ID
instance showITEM_ID :: Show ITEM_ID where show _ = "orders.item_id"
instance columnITEM_ID :: Column ITEM_ID Int
instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where from_ = fromTable
instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
instance colOfAAAOrdersITEM_ID :: ColOf ITEM_ID (TABLE Orders a) Int where from_ = getPropAndDecode
-- order_notes :: Column (Maybe String)
-- order_notes = makeColumn orders "order_notes"
data ORDER_NOTES = ORDER_NOTES
instance showORDER_NOTES :: Show ORDER_NOTES where show _ = "orders.order_notes"
instance columnORDER_NOTES :: Column ORDER_NOTES String
instance colOfTABLE_ORDER_NOTES :: (ColOf ORDER_NOTES b String) => ColOf ORDER_NOTES (TABLE a b) String where from_ = fromTable
instance colOfINNER_JOIN_ORDER_NOTES :: (ColOf ORDER_NOTES a String) => ColOf ORDER_NOTES (INNER_JOIN a) String where from_ = fromInnerJoin
instance colOfAAAOrdersORDER_NOTES :: ColOf ORDER_NOTES (TABLE Orders a) String where from_ = getPropAndDecode
-- order_user_id :: Column Int
-- order_user_id = makeColumn orders "user_id"
data ORDER_USER_ID = ORDER_USER_ID
instance showORDER_USER_ID :: Show ORDER_USER_ID where show _ = "orders.user_id"
instance columnORDER_USER_ID :: Column ORDER_USER_ID Int
instance colOfTABLE_ORDER_USER_ID :: (ColOf ORDER_USER_ID b Int) => ColOf ORDER_USER_ID (TABLE a b) Int where from_ = fromTable
instance colOfINNER_JOIN_ORDER_USER_ID :: (ColOf ORDER_USER_ID a Int) => ColOf ORDER_USER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
instance colOfAAAOrdersORDER_USER_ID :: ColOf ORDER_USER_ID (TABLE Orders a) Int where from_ = getPropAndDecode

-- users :: Table
-- users = Table "users"
data Users = Users
instance showUsers :: Show Users where show _ = "users"

users :: forall a. a -> TABLE Users a
users = TABLE Users
--
-- user_id :: Column Int
-- user_id = makeColumn users "user_id"
data USER_ID = USER_ID
instance columnUSER_ID :: Column USER_ID Int
instance showUSER_ID :: Show USER_ID where show _ = "users.user_id"
instance colOfTABLE_USER_ID :: (ColOf USER_ID b Int) => ColOf USER_ID (TABLE a b) Int where from_ = fromTable
instance colOfINNER_JOIN_USER_ID :: (ColOf USER_ID a Int) => ColOf USER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
instance colOfAAAUsersUSER_ID :: ColOf USER_ID (TABLE Users a) Int where from_ = getPropAndDecode
--
-- name :: Column (Maybe String)
-- name = makeColumn users "name"
data NAME = NAME
instance columnNAME :: Column NAME (Maybe String)
instance showNAME :: Show NAME where show _ = "users.name"
instance colOfTABLE_NAME :: (ColOf NAME b (Maybe String) ) => ColOf NAME (TABLE a b) (Maybe String)  where from_ = fromTable
instance colOfINNER_JOIN_NAME :: (ColOf NAME a (Maybe String) ) => ColOf NAME (INNER_JOIN a) (Maybe String)  where from_ = fromInnerJoin
instance colOfAAAUsersNAME :: ColOf NAME (TABLE Users a) (Maybe String) where from_ = getPropAndDecode
-- email :: Column String
-- email = makeColumn users "email"
data EMAIL = EMAIL
instance columnEMAIL :: Column EMAIL String
instance showEMAIL :: Show EMAIL where show _ = "users.email"
instance colOfTABLE_EMAIL :: (ColOf EMAIL b String ) => ColOf EMAIL (TABLE a b) String  where from_ = fromTable
instance colOfINNER_JOIN_EMAIL :: (ColOf EMAIL a String ) => ColOf EMAIL (INNER_JOIN a) String  where from_ = fromInnerJoin
instance colOfAAAUsersEMAIL :: ColOf EMAIL (TABLE Users a) String where from_ = getPropAndDecode
-- registered :: Column Boolean
-- registered = makeColumn users "registered"
data REGISTERED = REGISTERED
instance columnREGISTERED :: Column REGISTERED Boolean
instance showREGISTERED :: Show REGISTERED where show _ = "users.registered"
instance colOfTABLE_REGISTERED :: (ColOf REGISTERED b Boolean ) => ColOf REGISTERED (TABLE a b) Boolean  where from_ = fromTable
instance colOfINNER_JOIN_REGISTERED :: (ColOf REGISTERED a Boolean ) => ColOf REGISTERED (INNER_JOIN a) Boolean  where from_ = fromInnerJoin
instance colOfAAAUsersREGISTERED :: ColOf REGISTERED (TABLE Users a) Boolean where from_ = getPropAndDecode


-- instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where
--   from_ ITEM_ID (TABLE a b) = from_ ITEM_ID b
--
-- instance colOfOrdersITEM_ID :: ColOf ITEM_ID (TABLE Orders a) Int where
--   from_ t _ = \f -> f ! (show t) >>= decode_
--
-- instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where
--   from_ ITEM_ID (INNER_JOIN (On _ a)) = from_ ITEM_ID a
--
-- data ORDER_NAME = ORDER_NAME
--
-- instance showORDER_NAME :: Show ORDER_NAME where show _ = "orders.order_name"
--
-- instance columnORDER_NAME :: Column ORDER_NAME String
--
-- instance colOfTABLE_ORDER_NAME :: (ColOf ORDER_NAME b String) => ColOf ORDER_NAME (TABLE a b) String where
--   from_ ORDER_NAME (TABLE a b) = from_ ORDER_NAME b
--
-- instance colOfOrdersORDER_NAME :: ColOf ORDER_NAME (TABLE Orders a) String where
--   from_ t _ = \f -> f ! (show t) >>= decode_
--
-- instance colOfINNER_JOIN_ORDER_NAME :: (ColOf ORDER_NAME a String) => ColOf ORDER_NAME (INNER_JOIN a) String where
--   from_ ORDER_NAME (INNER_JOIN (On _ a)) = from_ ORDER_NAME a
--
-- data ITEM_ID = ITEM_ID
--
-- instance showITEM_ID :: Show ITEM_ID where show _ = "items.item_id"
--
-- instance columnITEM_ID :: Column ITEM_ID Int
--
-- instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where
--   from_ ITEM_ID (TABLE a b) = from_ ITEM_ID b
--
-- instance colOfItemsITEM_ID :: ColOf ITEM_ID (TABLE Items a) Int where
--   from_ t _ f = f ! (show t) >>= decode_
--
-- instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where
--   from_ ITEM_ID (INNER_JOIN (On _ a)) = from_ ITEM_ID a

-- submissions :: forall t1. t1 -> TABLE Submissions t1
-- submissions = TABLE Submissions
-- items :: forall t5. t5 -> TABLE Items t5
-- items = TABLE Items
--
--
-- res2 :: SelectQuery
--   { order_id :: Int
--   , order_name :: String
--   }
-- res2 = select $ (cols_ f)
--   {order_id:_, order_name:_} .>>
--   ORDER_ID >> ORDER_NAME
--     where
--       f = (
--         from $ orders $
--         inner_join items (on ITEM_ID ITEM_ID) unit
--         )
--         # whereVal ITEM_ID 8
--         # whereSub ITEM_ID ORDER_ID
--           ((from $ orders unit) # whereVal ORDER_ID 9)
--         # order_by ORDER_NAME ASC
