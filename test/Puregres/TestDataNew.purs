module Test.Puregres.TestDataNew where

import Prelude

import Puregres.SelectCols (INNER_JOIN)
import Puregres.Class (class Col, class ColOf)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Puregres.PuregresSqlValue (decode_)
import Puregres.Type (TABLE(..))

data Orders = Orders
instance showOrders :: Show Orders where show _ = "orders"

orders :: forall a. a -> TABLE Orders a
orders = TABLE Orders

data ORDER_ID = ORDER_ID

instance showORDER_ID :: Show ORDER_ID where
  show _ = "orders.order_id"

instance colORDER_ID :: Col ORDER_ID ({order_id :: Int}) Int where
  getF _ f = do
    order_id <- f ! "order_id" >>= decode_
    pure $ {order_id}
  colName _ = "order_id"

instance colOfAAAORDER_IDOrders :: ColOf ORDER_ID ({order_id :: Int}) Int (TABLE Orders n)
instance colOfTABLE_ORDER_ID :: (ColOf ORDER_ID ({order_id :: Int}) Int b ) => ColOf ORDER_ID ({order_id :: Int}) Int (TABLE a b)
instance colOfORDER_IDINNER_JOIN :: (ColOf ORDER_ID ({order_id :: Int}) Int n) => ColOf ORDER_ID ({order_id :: Int}) Int (INNER_JOIN n)

data ITEM_ID = ITEM_ID

instance showITEM_ID :: Show ITEM_ID where
  show _ = "orders.item_id"

instance colITEM_ID :: Col ITEM_ID ({item_id :: Int}) Int where
  getF _ f = do
    item_id <- f ! "item_id" >>= decode_
    pure $ {item_id}
  colName _ = "item_id"

instance colOfAAAITEM_IDOrders :: ColOf ITEM_ID ({item_id :: Int}) Int (TABLE Orders n)
instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID ({item_id :: Int}) Int b ) => ColOf ITEM_ID ({item_id :: Int}) Int (TABLE a b)
instance colOfITEM_IDINNER_JOIN :: (ColOf ITEM_ID ({item_id :: Int}) Int n) => ColOf ITEM_ID ({item_id :: Int}) Int (INNER_JOIN n)

data ORDER_NOTES = ORDER_NOTES

instance showORDER_NOTES :: Show ORDER_NOTES where
  show _ = "orders.order_notes"

instance colORDER_NOTES :: Col ORDER_NOTES ({order_notes :: String}) String where
  getF _ f = do
    order_notes <- f ! "order_notes" >>= decode_
    pure $ {order_notes}
  colName _ = "order_notes"

instance colOfAAAORDER_NOTESOrders :: ColOf ORDER_NOTES ({order_notes :: String}) String (TABLE Orders n)
instance colOfTABLE_ORDER_NOTES :: (ColOf ORDER_NOTES ({order_notes :: String}) String b ) => ColOf ORDER_NOTES ({order_notes :: String}) String (TABLE a b)
instance colOfORDER_NOTESINNER_JOIN :: (ColOf ORDER_NOTES ({order_notes :: String}) String n) => ColOf ORDER_NOTES ({order_notes :: String}) String (INNER_JOIN n)

data ORDER_USER_ID = ORDER_USER_ID

instance showORDER_USER_ID :: Show ORDER_USER_ID where
  show _ = "orders.user_id"

instance colORDER_USER_ID :: Col ORDER_USER_ID ({user_id :: Int}) Int where
  getF _ f = do
    user_id <- f ! "user_id" >>= decode_
    pure $ {user_id}
  colName _ = "user_id"

instance colOfAAAORDER_USER_IDOrders :: ColOf ORDER_USER_ID ({user_id :: Int}) Int (TABLE Orders n)
instance colOfTABLE_ORDER_USER_ID :: (ColOf ORDER_USER_ID ({user_id :: Int}) Int b ) => ColOf ORDER_USER_ID ({user_id :: Int}) Int (TABLE a b)
instance colOfORDER_USER_IDINNER_JOIN :: (ColOf ORDER_USER_ID ({user_id :: Int}) Int n) => ColOf ORDER_USER_ID ({user_id :: Int}) Int (INNER_JOIN n)

data Users = Users
instance showUsers :: Show Users where show _ = "users"

users :: forall a. a -> TABLE Users a
users = TABLE Users

data USER_ID = USER_ID

instance showUSER_ID :: Show USER_ID where
  show _ = "users.user_id"

instance colUSER_ID :: Col USER_ID ({user_id :: Int}) Int where
  getF _ f = do
    user_id <- f ! "user_id" >>= decode_
    pure $ {user_id}
  colName _ = "user_id"

instance colOfAAAUSER_IDOrders :: ColOf USER_ID ({user_id :: Int}) Int (TABLE Users n)
instance colOfTABLE_USER_ID :: (ColOf USER_ID ({user_id :: Int}) Int b ) => ColOf USER_ID ({user_id :: Int}) Int (TABLE a b)
instance colOfUSER_IDINNER_JOIN :: (ColOf USER_ID ({user_id :: Int}) Int n) => ColOf USER_ID ({user_id :: Int}) Int (INNER_JOIN n)

data NAME = NAME

instance showNAME :: Show NAME where
  show _ = "users.name"

instance colNAME :: Col NAME ({name :: (Maybe String)}) (Maybe String) where
  getF _ f = do
    name <- f ! "name" >>= decode_
    pure $ {name}
  colName _ = "name"

instance colOfAAANAMEOrders :: ColOf NAME ({name :: (Maybe String)}) (Maybe String) (TABLE Users n)
instance colOfTABLE_NAME :: (ColOf NAME ({name :: (Maybe String)}) (Maybe String) b ) => ColOf NAME ({name :: (Maybe String)}) (Maybe String) (TABLE a b)
instance colOfNAMEINNER_JOIN :: (ColOf NAME ({name :: (Maybe String)}) (Maybe String) n) => ColOf NAME ({name :: (Maybe String)}) (Maybe String) (INNER_JOIN n)

data EMAIL = EMAIL

instance showEMAIL :: Show EMAIL where
  show _ = "users.email"

instance colEMAIL :: Col EMAIL ({email :: String}) String where
  getF _ f = do
    email <- f ! "email" >>= decode_
    pure $ {email}
  colName _ = "email"

instance colOfAAAEMAILOrders :: ColOf EMAIL ({email :: String}) String (TABLE Users n)
instance colOfTABLE_EMAIL :: (ColOf EMAIL ({email :: String}) String b ) => ColOf EMAIL ({email :: String}) String (TABLE a b)
instance colOfEMAILINNER_JOIN :: (ColOf EMAIL ({email :: String}) String n) => ColOf EMAIL ({email :: String}) String (INNER_JOIN n)

data REGISTERED = REGISTERED

instance showREGISTERED :: Show REGISTERED where
  show _ = "users.registered"

instance colREGISTERED :: Col REGISTERED ({registered :: Boolean}) Boolean where
  getF _ f = do
    registered <- f ! "registered" >>= decode_
    pure $ {registered}
  colName _ = "registered"

instance colOfAAAREGISTEREDOrders :: ColOf REGISTERED ({registered :: Boolean}) Boolean (TABLE Users n)
instance colOfTABLE_REGISTERED :: (ColOf REGISTERED ({registered :: Boolean}) Boolean b ) => ColOf REGISTERED ({registered :: Boolean}) Boolean (TABLE a b)
instance colOfREGISTEREDINNER_JOIN :: (ColOf REGISTERED ({registered :: Boolean}) Boolean n) => ColOf REGISTERED ({registered :: Boolean}) Boolean (INNER_JOIN n)
