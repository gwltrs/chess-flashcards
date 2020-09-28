module Test.Main where

import Prelude (Unit, discard)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Types (Action(..), FEN, View(..), Alert(..))
import Reducer (reducer)

main :: Effect Unit
main = runTest do
  suite "Reducer" do

    test "User creates New file" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing } 
          NewFile
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing }

    test "User types in the Name field" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
          (UpdatePuzzleName "Opera Hou")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "", alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "", alert: Nothing } 
          (UpdatePuzzleName "Opera House Checkmate")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" "", alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" ohcFEN, alert: Nothing } 
          (UpdatePuzzleName "OHC")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" ohcFEN, alert: Nothing }

    test "User types in the FEN field" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
          (UpdateFEN ohcFEN)
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" ohcFEN, alert: Nothing } 
          (UpdateFEN "not real FEN")
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "not real FEN", alert: Nothing }

    test "User tries to create puzzle with empty field(s)" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" "", alert: Nothing } 
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu " " ohcFEN, alert: Nothing } -- Should check for length after trimming
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" "   ", alert: Nothing } -- Should check for length after trimming
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }

ohcFEN :: FEN
ohcFEN = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -"

ohcFENWithMoveNumbers :: FEN
ohcFENWithMoveNumbers = "asdfasf"