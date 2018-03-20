module Test.Puregres.Select where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Puregres.Comparator (Comparator(..))
import Puregres.Select (Direction(..), SelectEnded, endSelect, from, inner_join, on, order_by, select, whereAny, whereQuery, whereVal, (++))
import Test.Puregres.TestDataNew (EMAIL(..), ITEM_ID(..), NAME(..), ORDER_ID(..), ORDER_USER_ID(..), REGISTERED(..), REVIEW_ID(..), USER_ID(..), orders, reviews, users)
import Test.TestUtils (getParamStrings, getParamTypes)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

runTest :: forall a. Free (TestF a) Unit
runTest = suite "Test.Puregres.Select" do
  suite "select function" do
    suite "should return a SelectQuery, shown as a sql string and containing the correct params" do

      test "simpleSelect" do

        Assert.equal expectedShowSimpleSelect $ show simpleSelect
        Assert.equal [] (getParamStrings simpleSelect)
        Assert.equal [] (getParamTypes simpleSelect)

      test "multiColumSelect" do

        Assert.equal expectedShowMultiColumnSelect $ show multiColumSelect

      test "innerJoinSelect" do

        Assert.equal expectedShowInnerJoinSelect $ show innerJoinSelect

      test "innerJoinMultipleSelect" do

        Assert.equal expectedShowInnerJoinMultipleSelect $ show innerJoinMultipleSelect

      test "whereSelect" do

        Assert.equal expectedShowWhereSelect $ show whereSelect
        Assert.equal ["null", "true"] (getParamStrings whereSelect)
        Assert.equal ["Null", "Boolean"] (getParamTypes whereSelect)

      test "orderBySelect" do

        Assert.equal expectedShowOrderBySelect $ show orderBySelect

      test "whereSubQuerySelect" do

        Assert.equal expectedShowWhereSubQuerySelect $ show whereSubQuerySelect
        Assert.equal ["true", "10", "100", "abc@gmail.com"] (getParamStrings whereSubQuerySelect)
        Assert.equal ["Boolean", "Number", "Number", "String"] (getParamTypes whereSubQuerySelect)

      test "whereAnyIntSelect" do

        Assert.equal expectedShowWhereAnyIntSelect $ show whereAnyIntSelect
        Assert.equal ["10,20,30"] (getParamStrings whereAnyIntSelect)
        Assert.equal ["Array"] (getParamTypes whereAnyIntSelect)

      test "whereAnyStringSelect" do

        Assert.equal expectedShowWhereAnyStringSelect $ show whereAnyStringSelect
        Assert.equal ["abc@gmail.com,xyz@gmail.com"] (getParamStrings whereAnyStringSelect)
        Assert.equal ["Array"] (getParamTypes whereAnyStringSelect)


simpleSelect :: SelectEnded
  { email :: String
  }
simpleSelect = endSelect $
  select
    EMAIL
    # from users

expectedShowSimpleSelect :: String
expectedShowSimpleSelect = "SELECT\n    users.email\nFROM users"

multiColumSelect :: SelectEnded
  { email :: String
  , name :: Maybe String
  , registered :: Boolean
  }
multiColumSelect = endSelect $
   select
     EMAIL ++
     NAME ++
     REGISTERED
     # from users

expectedShowMultiColumnSelect :: String
expectedShowMultiColumnSelect =
  """SELECT
    users.email,
    users.name,
    users.registered
FROM users"""


innerJoinSelect :: SelectEnded
  { email :: String
  , order_id :: Int
  }
innerJoinSelect = endSelect $
  select
    EMAIL ++
    ORDER_ID
  # from (
      users <<<
      inner_join orders (on USER_ID Eq ORDER_USER_ID)
    )

expectedShowInnerJoinSelect :: String
expectedShowInnerJoinSelect =
  """SELECT
    users.email,
    orders.order_id
FROM users
  INNER JOIN orders ON users.user_id = orders.user_id"""

innerJoinMultipleSelect :: SelectEnded
  { email :: String
  , order_id :: Int
  }
innerJoinMultipleSelect = endSelect $
  select
    EMAIL ++
    ORDER_ID
  # from (
      users <<<
      inner_join orders (on USER_ID Eq ORDER_USER_ID) <<<
      inner_join reviews (on ORDER_USER_ID Eq REVIEW_ID)
    )

expectedShowInnerJoinMultipleSelect :: String
expectedShowInnerJoinMultipleSelect =
  """SELECT
    users.email,
    orders.order_id
FROM users
  INNER JOIN orders ON users.user_id = orders.user_id
  INNER JOIN reviews ON orders.user_id = reviews.review_id"""

whereSelect :: SelectEnded
  { user_id :: Int
  , order_id :: Int
  }
whereSelect = endSelect $
  select
    USER_ID ++
    ORDER_ID
  # from (
      users <<<
      inner_join orders (on USER_ID Eq ORDER_USER_ID)
    )
  # whereVal NAME Eq Nothing
  # whereVal REGISTERED Eq true

expectedShowWhereSelect :: String
expectedShowWhereSelect =
  """SELECT
    users.user_id,
    orders.order_id
FROM users
  INNER JOIN orders ON users.user_id = orders.user_id
WHERE users.name = $1
AND users.registered = $2"""

orderBySelect :: SelectEnded
  { user_id :: Int
  , order_id :: Int
  }
orderBySelect = endSelect $
  select
    USER_ID ++
    ORDER_ID
  # from (
      users <<<
      inner_join orders (on USER_ID Eq ORDER_USER_ID)
    )
  # order_by EMAIL ASC
  # order_by REGISTERED DESC

expectedShowOrderBySelect :: String
expectedShowOrderBySelect =
  """SELECT
    users.user_id,
    orders.order_id
FROM users
  INNER JOIN orders ON users.user_id = orders.user_id
ORDER BY
  users.email ASC,
  users.registered DESC"""

whereSubQuerySelect :: SelectEnded
  { email :: String
  }
whereSubQuerySelect = endSelect $
  select EMAIL
  # from users
  # whereVal REGISTERED Eq true
  # whereQuery USER_ID Eq (
      select ORDER_USER_ID
      # from orders
      # whereVal ITEM_ID Gt 10
      # whereVal ITEM_ID Lt 100
    )
  # whereVal EMAIL Eq "abc@gmail.com"

expectedShowWhereSubQuerySelect :: String
expectedShowWhereSubQuerySelect =
  """SELECT
    users.email
FROM users
WHERE users.registered = $1
AND users.user_id = (
SELECT
    orders.user_id
FROM orders
WHERE orders.item_id > $2
AND orders.item_id < $3)
AND users.email = $4"""

whereAnyIntSelect :: SelectEnded
  { email :: String
  }
whereAnyIntSelect = endSelect $
  select EMAIL
  # from users
  # whereAny USER_ID Eq [10, 20, 30]

expectedShowWhereAnyIntSelect :: String
expectedShowWhereAnyIntSelect =
  """SELECT
    users.email
FROM users
WHERE users.user_id = ANY($1::int[])"""


whereAnyStringSelect :: SelectEnded
  { user_id :: Int
  }
whereAnyStringSelect = endSelect $
  select USER_ID
  # from users
  # whereAny EMAIL Eq ["abc@gmail.com", "xyz@gmail.com"]

expectedShowWhereAnyStringSelect :: String
expectedShowWhereAnyStringSelect =
  """SELECT
    users.user_id
FROM users
WHERE users.email = ANY($1::varchar[])"""
