module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)

import ReducerTests (reducerTests)
import PuzzlesJSONTests (puzzlesJSONTests)
import RenderTests (renderTests)

main :: Effect Unit
main = runTest do
  reducerTests
  puzzlesJSONTests
  renderTests