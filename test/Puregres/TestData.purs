module Test.Puregres.TestData where
--
-- import Prelude
--
-- import Puregres.Class (class ColOf, class Column)
-- import Data.Maybe (Maybe)
-- import Puregres.Select (INNER_JOIN, fromInnerJoin, fromTable, getPropAndDecode)
-- import Puregres.Type (TABLE(..))
--
-- data Orders = Orders
-- instance showOrders :: Show Orders where show _ = "orders"
--
-- orders :: forall a. a -> TABLE Orders a
-- orders = TABLE Orders
--
-- data ORDER_ID = ORDER_ID
-- instance columnORDER_ID :: Column ORDER_ID Int where colName _ = "order_id"
-- instance showORDER_ID :: Show ORDER_ID where show _ = "orders.order_id"
-- instance colOfTABLE_ORDER_ID :: (ColOf ORDER_ID b Int) => ColOf ORDER_ID (TABLE a b) Int where from_ = fromTable
-- instance colOfINNER_JOIN_ORDER_ID :: (ColOf ORDER_ID a Int) => ColOf ORDER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
-- instance colOfAAAOrdersORDER_ID :: ColOf ORDER_ID (TABLE Orders a) Int where from_ = getPropAndDecode
--
--
-- data ITEM_ID = ITEM_ID
-- instance showITEM_ID :: Show ITEM_ID where show _ = "orders.item_id"
-- instance columnITEM_ID :: Column ITEM_ID Int where colName _ = "item_id"
-- instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where from_ = fromTable
-- instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
-- instance colOfAAAOrdersITEM_ID :: ColOf ITEM_ID (TABLE Orders a) Int where from_ = getPropAndDecode
--
--
-- data ORDER_NOTES = ORDER_NOTES
-- instance showORDER_NOTES :: Show ORDER_NOTES where show _ = "orders.order_notes"
-- instance columnORDER_NOTES :: Column ORDER_NOTES String where colName _ = "order_notes"
-- instance colOfTABLE_ORDER_NOTES :: (ColOf ORDER_NOTES b String) => ColOf ORDER_NOTES (TABLE a b) String where from_ = fromTable
-- instance colOfINNER_JOIN_ORDER_NOTES :: (ColOf ORDER_NOTES a String) => ColOf ORDER_NOTES (INNER_JOIN a) String where from_ = fromInnerJoin
-- instance colOfAAAOrdersORDER_NOTES :: ColOf ORDER_NOTES (TABLE Orders a) String where from_ = getPropAndDecode
--
--
-- data ORDER_USER_ID = ORDER_USER_ID
-- instance showORDER_USER_ID :: Show ORDER_USER_ID where show _ = "orders.user_id"
-- instance columnORDER_USER_ID :: Column ORDER_USER_ID Int where colName _ = "user_id"
-- instance colOfTABLE_ORDER_USER_ID :: (ColOf ORDER_USER_ID b Int) => ColOf ORDER_USER_ID (TABLE a b) Int where from_ = fromTable
-- instance colOfINNER_JOIN_ORDER_USER_ID :: (ColOf ORDER_USER_ID a Int) => ColOf ORDER_USER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
-- instance colOfAAAOrdersORDER_USER_ID :: ColOf ORDER_USER_ID (TABLE Orders a) Int where from_ = getPropAndDecode
--
--
-- data Users = Users
-- instance showUsers :: Show Users where show _ = "users"
--
-- users :: forall a. a -> TABLE Users a
-- users = TABLE Users
--
--
-- data USER_ID = USER_ID
-- instance columnUSER_ID :: Column USER_ID Int where colName _ = "user_id"
-- instance showUSER_ID :: Show USER_ID where show _ = "users.user_id"
-- instance colOfTABLE_USER_ID :: (ColOf USER_ID b Int) => ColOf USER_ID (TABLE a b) Int where from_ = fromTable
-- instance colOfINNER_JOIN_USER_ID :: (ColOf USER_ID a Int) => ColOf USER_ID (INNER_JOIN a) Int where from_ = fromInnerJoin
-- instance colOfAAAUsersUSER_ID :: ColOf USER_ID (TABLE Users a) Int where from_ = getPropAndDecode
--
--
-- data NAME = NAME
-- instance columnNAME :: Column NAME (Maybe String) where colName _ = "name"
-- instance showNAME :: Show NAME where show _ = "users.name"
-- instance colOfTABLE_NAME :: (ColOf NAME b (Maybe String) ) => ColOf NAME (TABLE a b) (Maybe String)  where from_ = fromTable
-- instance colOfINNER_JOIN_NAME :: (ColOf NAME a (Maybe String) ) => ColOf NAME (INNER_JOIN a) (Maybe String)  where from_ = fromInnerJoin
-- instance colOfAAAUsersNAME :: ColOf NAME (TABLE Users a) (Maybe String) where from_ = getPropAndDecode
--
--
-- data EMAIL = EMAIL
-- instance columnEMAIL :: Column EMAIL String where colName _ = "email"
-- instance showEMAIL :: Show EMAIL where show _ = "users.email"
-- instance colOfTABLE_EMAIL :: (ColOf EMAIL b String ) => ColOf EMAIL (TABLE a b) String  where from_ = fromTable
-- instance colOfINNER_JOIN_EMAIL :: (ColOf EMAIL a String ) => ColOf EMAIL (INNER_JOIN a) String  where from_ = fromInnerJoin
-- instance colOfAAAUsersEMAIL :: ColOf EMAIL (TABLE Users a) String where from_ = getPropAndDecode
--
--
-- data REGISTERED = REGISTERED
-- instance columnREGISTERED :: Column REGISTERED Boolean where colName _ = "registered"
-- instance showREGISTERED :: Show REGISTERED where show _ = "users.registered"
-- instance colOfTABLE_REGISTERED :: (ColOf REGISTERED b Boolean ) => ColOf REGISTERED (TABLE a b) Boolean  where from_ = fromTable
-- instance colOfINNER_JOIN_REGISTERED :: (ColOf REGISTERED a Boolean ) => ColOf REGISTERED (INNER_JOIN a) Boolean  where from_ = fromInnerJoin
-- instance colOfAAAUsersREGISTERED :: ColOf REGISTERED (TABLE Users a) Boolean where from_ = getPropAndDecode
