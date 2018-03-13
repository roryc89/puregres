module Test.Puregres.Insert where

import Prelude (Unit, discard, show, (#), ($))
import Puregres.Insert (InsertInto, InsertIntoReturning, insertInto, returning)
import Puregres.Type (EndQuery, TABLE)
import Test.Puregres.TestData (ITEM_ID(..), ORDER_ID(..), ORDER_NOTES(..), Orders, orders)
import Control.Monad.Free (Free)
import Data.Tuple.Nested ((/\))
import Puregres.InsertOrUpdateCols (col, (++))
import Test.TestUtils (getParamStrings, getParamTypes)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

runTest :: forall a. Free (TestF a) Unit
runTest = suite "Test.Puregres.Insert" do
  suite "insertInto function" do
    suite "should return an InsertInto, shown as a sql string and containing the correct params" do

      test "simpleInsert" do

        Assert.equal expectedShowSimpleInsert $ show simpleInsert
        Assert.equal ["10", "test notes"] (getParamStrings simpleInsert)
        Assert.equal ["Number", "String"] (getParamTypes simpleInsert)

      test "insertReturning" do

        Assert.equal expectedShowInsertReturning $ show insertReturning
        Assert.equal ["10", "test notes"] (getParamStrings insertReturning)
        Assert.equal ["Number", "String"] (getParamTypes insertReturning)

simpleInsert :: InsertInto (TABLE Orders EndQuery)
simpleInsert =
  insertInto
    orders
    (col (ITEM_ID /\ 10)
      ++ (ORDER_NOTES /\ "test notes")
    )

expectedShowSimpleInsert :: String
expectedShowSimpleInsert = """INSERT INTO orders
  (item_id, order_notes)
VALUES
  ($1, $2)
"""

insertReturning :: InsertIntoReturning (TABLE Orders EndQuery) ORDER_ID
insertReturning =
  insertInto
    orders
    (col (ITEM_ID /\ 10)
      ++ (ORDER_NOTES /\ "test notes")
    )
    # returning ORDER_ID

expectedShowInsertReturning :: String
expectedShowInsertReturning = """INSERT INTO orders
  (item_id, order_notes)
VALUES
  ($1, $2)
RETURNING order_id"""
