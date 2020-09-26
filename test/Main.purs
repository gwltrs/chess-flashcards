module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Types
import Reducer (reducer)

main :: Effect Unit
main = runTest do
  suite "Reducer" do

    test "User creates New file" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: LoadingFile } 
          NewFile
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "" }

    test "User types in the Name field" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "" "" } 
          (UpdatePuzzleName "Opera Hou")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "" }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "" } 
          (UpdatePuzzleName "Opera House Checkmate")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" "" }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" } 
          (UpdatePuzzleName "OHC")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" }