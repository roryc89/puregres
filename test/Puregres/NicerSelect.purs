module Test.Puregres.NicerSelect where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foreign (tagOf, toForeign)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Puregres.NicerSelect
import Puregres.Type (Table(..))
import Test.TestUtils (unsafeToStringJs)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

runTest :: forall a. Free (TestF a) Unit
runTest = suite "Test.Puregres.Select" do
    suite "select function" do
      suite "return value should be a Right contructor containing a SELECT, shown as a sql string for correctly formed queries" do

        test "simpleSelect" do

          Assert.assert "Either should be a Right constructor" (isRight simpleSelect)
          Assert.equal expectedShowSimpleSelect (either (const "") show simpleSelect)

        test "multiColumSelect (with flipped functions)" do

          Assert.assert "multiColumSelect Either  should be a Right constructor" (isRight multiColumSelect)
          Assert.equal expectedShowMultiColumnSelect (either (const "") show multiColumSelect)

        test "innerJoinSelect" do

          Assert.assert "Either should be a Right constructor" (isRight innerJoinSelect)
          Assert.equal expectedShowInnerJoinSelect (either (const "") show innerJoinSelect)

        test "leftJoinSelect" do

          Assert.assert "Either should be a Right constructor" (isRight leftJoinSelect)
          Assert.equal expectedShowLeftJoinSelect (either (const "") show leftJoinSelect)

        test "whereSelect" do

          Assert.assert "Either should be a Right constructor" (isRight whereSelect)
          Assert.equal expectedShowWhereSelect (either (const "") show whereSelect)

        test "orderBySelect" do

          Assert.assert "Either should be a Right constructor" (isRight orderBySelect)
          Assert.equal expectedShowOrderBySelect (either (const "") show orderBySelect)

        test "whereSubQuerySelect" do

          Assert.assert "Either should be a Right constructor" (isRight whereSubQuerySelect)
          Assert.equal expectedShowWhereSubQuerySelect (either (const "") show whereSubQuerySelect)
    --
    --   -- TODO: figure out how to encode these into the type system to make them impossible
    --   suite "select result should be a Left contructor if the query is invalid" do
    --
    --     test "query with column not in table" do
    --
    --       let query = order_id <** {order_id:_} `From` (TABLE users)
    --       Assert.assert "Either should be a Left constructor" (isLeft query)
    --
    --     test "query using maybe column combinator when it should not" do
    --
    --       let query = email <?? {email:_} `From` (TABLE users)
    --       Assert.assert "Either should be a Left constructor" (isLeft query)
    --
    --     test "query not using maybe column combinator when it should" do
    --
    --       let query = email <** {email:_} `From` (TABLE orders `LEFT_JOIN` (users `on` (order_user_id `eqC` user_id)))
    --       Assert.assert "Either should be a Left constructor" (isLeft query)
    --
    -- suite "getParams function should give the params of a SELECT" do
    --
    --   test "simpleSelect" do
    --    Assert.equal (Right []) (getParamStrings simpleSelect)
    --    Assert.equal (Right []) (getParamTypes simpleSelect)
    --
    --   test "whereSelect" do
    --    Assert.equal (Right ["testemail@gmail.com", "true"]) (getParamStrings whereSelect)
    --    Assert.equal (Right ["String", "Boolean"]) (getParamTypes whereSelect)

getParamStrings :: forall a b. Functor a => a (SELECT b) -> a (Array String)
getParamStrings = map (getParams >>> (map unsafeToStringJs))

getParamTypes :: forall a b. Functor a => a (SELECT b) -> a (Array String)
getParamTypes = map (getParams >>> (map (toForeign >>> tagOf)))

simpleSelect :: Either String ( SELECT { email :: String } )
simpleSelect =
  SelectInto {email:_}
    # col email
    # from_ users

expectedShowSimpleSelect :: String
expectedShowSimpleSelect = "SELECT\n    public.users.email\nFROM public.users"

multiColumSelect :: Either String ( SELECT { user_id :: Int, name :: Maybe String } )
multiColumSelect =
  SelectInto { user_id:_, name:_}
    # col user_id &* name
    # from_ users

expectedShowMultiColumnSelect :: String
expectedShowMultiColumnSelect =
  """SELECT
    public.users.user_id,
    public.users.name
FROM public.users"""

infixr 2 apply as |>


innerJoinSelect :: Either String ( SELECT { order_id :: Int, email :: String } )
innerJoinSelect =
  SelectInto {email:_, order_id:_}
    # col email &* order_id
    # from users
      (_ `INNER_JOIN` (orders `on` (user_id === order_user_id)))

expectedShowInnerJoinSelect :: String
expectedShowInnerJoinSelect =
  """SELECT
    public.users.email,
    public.orders.order_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id"""
-- --
leftJoinSelect :: Either String ( SELECT { order_id :: Maybe Int, email :: String } )
leftJoinSelect =
  SelectInto {email:_, order_id:_}
    # col email &? order_id
    # from users
      (_ `LEFT_JOIN` (orders `on` (user_id === order_user_id)))

expectedShowLeftJoinSelect :: String
expectedShowLeftJoinSelect =
  """SELECT
    public.users.email,
    public.orders.order_id
FROM public.users
  LEFT JOIN public.orders ON public.users.user_id = public.orders.user_id"""

whereSelect :: Either String ( SELECT { user_id :: Int, order_id :: Int } )
whereSelect =
  SelectInto {user_id:_, order_id:_}
    # col user_id &* order_id
    # from users
      (_ `INNER_JOIN` (orders `on` (user_id === order_user_id)))
    # where_
      [ order_notes === unit --- unit for null values
      , email === "testemail@gmail.com"
      , registered === true
      ]

expectedShowWhereSelect :: String
expectedShowWhereSelect =
  """SELECT
    public.users.user_id,
    public.orders.order_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id
WHERE public.orders.order_notes = NULL
AND public.users.email = $1
AND public.users.registered = $2"""

orderBySelect :: Either String ( SELECT { user_id :: Int, order_id :: Int } )
orderBySelect =
  SelectInto {order_id:_, user_id:_}
    # col order_id &* user_id
    # from users
      (_ `INNER_JOIN` (orders `on` (user_id === order_user_id)))
    # orderBy
      [ asc email
      , desc registered
      ]

expectedShowOrderBySelect :: String
expectedShowOrderBySelect =
  """SELECT
    public.orders.order_id,
    public.users.user_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id
ORDER BY
  public.users.email ASC,
  public.users.registered DESC"""

whereSubQuerySelect :: Either String ( SELECT { email :: String } )
whereSubQuerySelect =
  SelectInto {email:_}
    # col email
    # from_ users
    # where_
      [ user_id ===
          selectW order_user_id
          # fromW_ orders
          # whereW
            [ item_id === 10 ]
      ]

expectedShowWhereSubQuerySelect :: String
expectedShowWhereSubQuerySelect =
  """SELECT
    public.users.email
FROM public.users
WHERE public.users.user_id = (
SELECT
    public.orders.user_id
FROM public.orders
WHERE public.orders.item_id = $1)"""

orders :: Table
orders = Table "public.orders"

order_id :: Column Int
order_id = makeColumn orders "order_id"

item_id :: Column Int
item_id = makeColumn orders "item_id"

order_notes :: Column (Maybe String)
order_notes = makeColumn orders "order_notes"

order_user_id :: Column Int
order_user_id = makeColumn orders "user_id"

users :: Table
users = Table "public.users"

user_id :: Column Int
user_id = makeColumn users "user_id"

name :: Column (Maybe String)
name = makeColumn users "name"

email :: Column String
email = makeColumn users "email"

registered :: Column Boolean
registered = makeColumn users "registered"
