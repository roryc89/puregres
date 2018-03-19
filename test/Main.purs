module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Puregres.Select as Select
import Test.Puregres.Insert as Insert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall t1.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | t1
    )
    Unit
main = runTest do
  Select.runTest
  Insert.runTest
