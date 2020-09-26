module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Types
import Reducer (reducer)
import Test.Values

main :: Effect Unit
main = runTest do
  suite "Reducer" do
    test "User creates New file" do
      Assert.equal (reducer initialState NewFile) newFileState