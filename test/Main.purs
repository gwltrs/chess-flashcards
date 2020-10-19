module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)

import ReducerTests (reducerTests)

main :: Effect Unit
main = runTest do
  reducerTests