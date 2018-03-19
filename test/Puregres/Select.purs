module Test.Puregres.Select where
--
-- import Prelude
-- import Control.Monad.Free (Free)
-- import Data.Maybe (Maybe(..))
-- import Puregres.Class (params, class Params)
-- import Puregres.Type
-- import Puregres.Select (Direction(..), SelectQuery, cols_, from, fromC, inner_join, on, order_by, select, whereSub, whereVal, (.>>), (>>))
-- import Test.TestUtils (unsafeToStringJs, getParamStrings, getParamTypes)
-- import Test.Unit (TestF, suite, test)
-- import Test.Unit.Assert as Assert
--
-- import Test.Puregres.TestData
--
-- runTest :: forall a. Free (TestF a) Unit
-- runTest = suite "Test.Puregres.Select" do
--   suite "select function" do
--     suite "should return a SelectQuery, shown as a sql string and containing the correct params" do
-- 
--       test "simpleSelect" do
--
--         Assert.equal expectedShowSimpleSelect $ show simpleSelect
--         Assert.equal [] (getParamStrings simpleSelect)
--         Assert.equal [] (getParamTypes simpleSelect)
--
--       test "multiColumSelect" do
--
--         Assert.equal expectedShowMultiColumnSelect $ show multiColumSelect
--
--       test "innerJoinSelect" do
--
--         Assert.equal expectedShowInnerJoinSelect $ show innerJoinSelect
--
--       test "whereSelect" do
--
--         Assert.equal expectedShowWhereSelect $ show whereSelect
--         Assert.equal ["null", "true"] (getParamStrings whereSelect)
--         Assert.equal ["Null", "Boolean"] (getParamTypes whereSelect)
--
--       test "orderBySelect" do
--
--         Assert.equal expectedShowOrderBySelect $ show orderBySelect
--
--       test "whereSubQuerySelect" do
--
--         Assert.equal expectedShowWhereSubQuerySelect $ show whereSubQuerySelect
--         Assert.equal ["true", "10", "abc@gmail.com"] (getParamStrings whereSubQuerySelect)
--         Assert.equal ["Boolean", "Number", "String"] (getParamTypes whereSubQuerySelect)
--
--
--
-- simpleSelect :: SelectQuery { email :: String }
-- simpleSelect = select $ f
--   {email:_ } .>>
--   EMAIL
--   where
--     f = fromC $ users end
--
-- expectedShowSimpleSelect :: String
-- expectedShowSimpleSelect = "SELECT\n    users.email as \"users.email\"\nFROM users"
--
-- multiColumSelect :: SelectQuery { email :: String, name :: Maybe String }
-- multiColumSelect = select $ f
--     {email:_, name:_} .>>
--     EMAIL >> NAME
--     where
--       f = fromC $ users end
--
-- expectedShowMultiColumnSelect :: String
-- expectedShowMultiColumnSelect =
--   """SELECT
--     users.email as "users.email",
--     users.name as "users.name"
-- FROM users"""
--
-- innerJoinSelect :: SelectQuery {email :: String, order_id :: Int}
-- innerJoinSelect = select $ f
--   {email:_, order_id:_} .>>
--   EMAIL >> ORDER_ID
--   where
--     f = fromC $ users $
--       inner_join orders (on USER_ID ORDER_USER_ID) end
--
-- expectedShowInnerJoinSelect :: String
-- expectedShowInnerJoinSelect =
--   """SELECT
--     users.email as "users.email",
--     orders.order_id as "orders.order_id"
-- FROM users
--   INNER JOIN orders ON users.user_id = orders.user_id"""
--
-- whereSelect :: SelectQuery { user_id :: Int, order_id :: Int }
-- whereSelect =
--   select $ (cols_ f)
--   {user_id:_, order_id:_} .>>
--   USER_ID >> ORDER_ID
--   where
--     f = (
--         from $ users $
--           inner_join orders (on USER_ID ORDER_USER_ID) end
--       )
--       # whereVal NAME Nothing
--       # whereVal REGISTERED true
--
--
-- expectedShowWhereSelect :: String
-- expectedShowWhereSelect =
--   """SELECT
--     users.user_id as "users.user_id",
--     orders.order_id as "orders.order_id"
-- FROM users
--   INNER JOIN orders ON users.user_id = orders.user_id
-- WHERE users.name = $1
-- AND users.registered = $2"""
--
-- orderBySelect :: SelectQuery { user_id :: Int, order_id :: Int }
-- orderBySelect =
--   select $ (cols_ f)
--   {user_id:_, order_id:_} .>>
--   USER_ID >> ORDER_ID
--   where
--     f = (
--       from $ users $
--         inner_join orders (on USER_ID ORDER_USER_ID) end
--       )
--       # order_by EMAIL ASC
--       # order_by REGISTERED DESC
--
-- expectedShowOrderBySelect :: String
-- expectedShowOrderBySelect =
--   """SELECT
--     users.user_id as "users.user_id",
--     orders.order_id as "orders.order_id"
-- FROM users
--   INNER JOIN orders ON users.user_id = orders.user_id
-- ORDER BY
--   users.email ASC,
--   users.registered DESC"""
--
-- whereSubQuerySelect :: SelectQuery { email :: String }
-- whereSubQuerySelect =
--   select $ (cols_ f)
--   {email:_} .>>
--   EMAIL
--   where
--     f = (
--         from $ users end
--       )
--       # whereVal REGISTERED true
--       # whereSub USER_ID ORDER_USER_ID
--         ((from $ orders end)
--           # whereVal ITEM_ID 10)
--       # whereVal EMAIL "abc@gmail.com"
--
-- expectedShowWhereSubQuerySelect :: String
-- expectedShowWhereSubQuerySelect =
--   """SELECT
--     users.email as "users.email"
-- FROM users
-- WHERE users.registered = $1
-- AND users.user_id = (
-- SELECT
--     orders.user_id
-- FROM orders
-- WHERE orders.item_id = $2)
-- AND users.email = $3"""
