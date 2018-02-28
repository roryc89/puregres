module Test.Puregres.Select where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

run :: forall t1. Free (TestF t1) Unit
run = suite "Test.Puregres.Select" do
    test "should " do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
      Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      Assert.equal 4 (2 + 2)
