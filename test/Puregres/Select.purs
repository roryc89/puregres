module Test.Puregres.Select where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either, either, isLeft, isRight)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Maybe (Maybe)
import Puregres.Select (From(From), FromExpr(..), SELECT, eqC, fromF, is, on, select, where_, (&*), (&?), (*&), (**>), (<**), (<??), orderBy, asc, desc)
import Puregres.Type (Column, Table(..), makeColumn)
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

        test "leftJoinSelect" do

          Assert.assert "Either should be a Right constructor" (isRight leftJoinSelect)
          Assert.equal expectedShowLeftJoinSelect (either (const "") show leftJoinSelect)

        test "innerJoinSelect" do

          Assert.assert "Either should be a Right constructor" (isRight innerJoinSelect)
          Assert.equal expectedShowInnerJoinSelect (either (const "") show innerJoinSelect)

        test "whereSelect" do

          Assert.assert "Either should be a Right constructor" (isRight whereSelect)
          Assert.equal expectedShowWhereSelect (either (const "") show whereSelect)

        test "orderBySelect" do

          Assert.assert "Either should be a Right constructor" (isRight orderBySelect)
          Assert.equal expectedShowOrderBySelect (either (const "") show orderBySelect)

      -- TODO: figure out how to encode these into the type system to make them impossible
      suite "select result should be a Left contructor if the query is invalid" do

        test "query with column not in table" do

          let query = order_id <** {order_id:_} `From` (TABLE users)
          Assert.assert "Either should be a Left constructor" (isLeft query)

        test "query using maybe column combinator when it should not" do

          let query = email <?? {email:_} `From` (TABLE users)
          Assert.assert "Either should be a Left constructor" (isLeft query)

        test "query not using maybe column combinator when it should" do

          let query = email <** {email:_} `From` (TABLE orders `LEFT_JOIN` (users `on` (order_user_id `eqC` user_id)))
          Assert.assert "Either should be a Left constructor" (isLeft query)

simpleSelect :: Either String ( SELECT ( Column { email :: String } ) )
simpleSelect =
  select $
    email <** {email:_}
    `From` (TABLE users)

expectedShowSimpleSelect :: String
expectedShowSimpleSelect = "SELECT public.users.email\nFROM public.users"

multiColumSelect :: Either String ( SELECT ( Column { user_id :: Int, name :: NullOrUndefined String } ) )
multiColumSelect =
  select $
    (TABLE users) `fromF` -- Flipping reads less like sql but allows columns to line up
    { user_id:_, name:_} **>
      user_id *& name

expectedShowMultiColumnSelect :: String
expectedShowMultiColumnSelect =
  """SELECT public.users.user_id, public.users.name
FROM public.users"""

innerJoinSelect :: Either String ( SELECT ( Column { order_id :: Int, email :: String } ) )
innerJoinSelect = select $
  order_id &* email <** {email:_, order_id:_}
  `From` (TABLE users
    `INNER_JOIN` (orders `on` (user_id `eqC` order_user_id))
  )
expectedShowInnerJoinSelect :: String
expectedShowInnerJoinSelect =
  """SELECT public.users.email, public.orders.order_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id"""

leftJoinSelect :: Either String ( SELECT ( Column { order_id :: Maybe Int, email :: String } ) )
leftJoinSelect = select $
  order_id &? email <** {email:_, order_id:_}
  `From` (TABLE users
    `LEFT_JOIN` (orders `on` (user_id `eqC` order_user_id))
  )
expectedShowLeftJoinSelect :: String
expectedShowLeftJoinSelect =
  """SELECT public.users.email, public.orders.order_id
FROM public.users
  LEFT JOIN public.orders ON public.users.user_id = public.orders.user_id"""

whereSelect :: Either String ( SELECT ( Column { user_id :: Int, order_id :: Int } ) )
whereSelect = select (
    user_id &* order_id <** {order_id:_, user_id:_}
    `From` (TABLE users
      `INNER_JOIN` (orders `on` (user_id `eqC` order_user_id))
    )
  )
  # where_
    [ email `is` "testemail@gmail.com"
    , registered `is` true
    ]

expectedShowWhereSelect :: String
expectedShowWhereSelect =
  """SELECT public.orders.order_id, public.users.user_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id
WHERE public.users.email = $1
AND public.users.registered = $2"""

orderBySelect :: Either String ( SELECT ( Column { user_id :: Int, order_id :: Int } ) )
orderBySelect = select (
    user_id &* order_id <** {order_id:_, user_id:_}
    `From` (TABLE users
      `INNER_JOIN` (orders `on` (user_id `eqC` order_user_id))
    )
  )
  # orderBy
    [ asc email
    , desc registered
    ]

expectedShowOrderBySelect :: String
expectedShowOrderBySelect =
  """SELECT public.orders.order_id, public.users.user_id
FROM public.users
  INNER JOIN public.orders ON public.users.user_id = public.orders.user_id
ORDER BY
  public.users.email ASC,
  public.users.registered DESC"""

orders :: Table
orders = Table "public.orders"

order_id :: Column Int
order_id = makeColumn orders "order_id"

item_id :: Column Int
item_id = makeColumn orders "item_id"

order_notes :: Column (NullOrUndefined String)
order_notes = makeColumn orders "order_notes"

order_user_id :: Column Int
order_user_id = makeColumn orders "user_id"

users :: Table
users = Table "public.users"

user_id :: Column Int
user_id = makeColumn users "user_id"

name :: Column (NullOrUndefined String)
name = makeColumn users "name"

email :: Column String
email = makeColumn users "email"

registered :: Column Boolean
registered = makeColumn users "registered"
