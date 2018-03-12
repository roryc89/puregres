module Test.Puregres.Insert where

import Prelude
import Control.Monad.Free (Free)
import Data.Foreign (tagOf, toForeign)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Puregres.Class (params)
import Puregres.Insert
import Puregres.Type
-- import Puregres.Type
import Test.TestUtils (unsafeToStringJs, getParamStrings, getParamTypes)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

import Test.Puregres.TestData

runTest :: forall a. Free (TestF a) Unit
runTest = suite "Test.Puregres.Insert" do
  suite "insertInto function" do
    suite "should return an InsertInto, shown as a sql string and containing the correct params" do

      test "simpleInsert" do

        Assert.equal expectedShowSimpleInsert $ show simpleInsert
        Assert.equal ["10", "test notes"] (getParamStrings simpleInsert)
        Assert.equal ["Number", "String"] (getParamTypes simpleInsert)

simpleInsert :: InsertInto (TABLE Orders EndQuery) ORDER_NOTES
simpleInsert =
  insertInto
    (orders end)
    (col (ITEM_ID /\ 10)
      ++ (ORDER_NOTES /\ "test notes")
    )

expectedShowSimpleInsert :: String
expectedShowSimpleInsert = """INSERT INTO orders
  (item_id, order_notes)
VALUES
  ($1, $2)
"""

-- insertWithReturn ::
