module PuzzlesJSONTests where

import Prelude (Unit, discard)
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

import PuzzlesJSON (makePuzzlesJSON, parsePuzzlesJSON)
import TestData

puzzlesJSONTests :: Free TestF Unit
puzzlesJSONTests = suite "PuzzlesJSON" do
  test "Puzzles are unchanged after being converted to and from JSON" do

    Assert.equal 
      (Just [])
      (parsePuzzlesJSON (makePuzzlesJSON []))

    Assert.equal 
      (Just twoEndgamePuzzles)
      (parsePuzzlesJSON (makePuzzlesJSON twoEndgamePuzzles))