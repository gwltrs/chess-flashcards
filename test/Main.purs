module Test.Main where

import Prelude (Unit, discard)

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Types (Action(..), FEN, View(..), Alert(..), Puzzle, Underpromotion(..))
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
        { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" "", alert: Nothing } 
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "", alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu " " ohcFEN, alert: Nothing } -- Should check for length after trimming
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu " " ohcFEN, alert: Just MissingNameOrFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" "   ", alert: Nothing } -- Should check for length after trimming
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "   ", alert: Just MissingNameOrFEN }

    test "User tries to create puzzle with invalid FEN" do

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseMissingInfo, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseMissingInfo, alert: Just InvalidFEN }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseNoWhiteKing, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseNoWhiteKing, alert: Just InvalidFEN }

    test "User tries to create puzzle with duplicate name" do

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame 1" ohcFENWithMoveNumbers, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame 1" ohcFENWithMoveNumbers, alert: Just DuplicateName }

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu " endgame 2 " ohcFENWithMoveNumbers, alert: Nothing } -- Making sure the check happens after the name is trimmed
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame 2 " ohcFENWithMoveNumbers, alert: Just DuplicateName }

    test "User tries to create puzzle with duplicate fen" do

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" endgamePuzzle1.fen, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" endgamePuzzle1.fen, alert: Just DuplicateFEN }

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" endgamePuzzle2.fen, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" (" " <> endgamePuzzle2.fen <> " "), alert: Just DuplicateFEN } -- Making sure the check happens after the FEN is trimmed

    test "User tries to create puzzle with duplicate name and FEN" do

      -- Duplicate name check should happen before duplicate FEN check

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame 1" endgamePuzzle1.fen, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame 1" endgamePuzzle1.fen, alert: Just DuplicateName }

    test "User navigates to create-puzzle view" do

      -- Both the puzzle name and FEN should be trimmed
      -- Move numbers from FEN should be stripped
      -- Unnecessary en passant info in FEN should be replaced with "-"
      -- Puzzle name "incrementing" feature
      --   This allows the user to name the puzzle with a description and an auto-increment number without having to remember the previous number
      --   "checkmate pattern ?" -> "checkmate pattern 1" if no puzzles already exist
      --   "endgame ?" -> "endgame 3" if two puzzles already exist with the names "endgame 1" and "endgame 2"

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "OHC" (" " <> ohcFEN <> "  "), alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "OHC" ohcFEN Nothing, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" (" " <> ohcFENWithMoveNumbers), alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: CreatingPuzzle "OHC" ohcFEN Nothing, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "   Open Game " openGameFENWithEnPassantAndMoveNumbers, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "Open Game" openGameFEN Nothing, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "   Open Game " openGameFENWithEnPassantAndMoveNumbers, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: [], reviewStack: [], view: CreatingPuzzle "Open Game" openGameFEN Nothing, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu " checkmate pattern ? " ohcFENWithMoveNumbers, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "checkmate pattern 1" ohcFEN Nothing, alert: Nothing }

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame ?" ohcFENWithMoveNumbers, alert: Nothing }
          CreatePuzzle
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "endgame 3" ohcFEN Nothing, alert: Nothing }

    test "User closes alert box" do

      Assert.equal 
        (reducer 
          { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu " endgame 1 " openGameFEN, alert: Just DuplicateName }
          CloseAlert
        ) 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu " endgame 1 " openGameFEN, alert: Nothing }

ohcFEN :: FEN
ohcFEN = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -"

ohcFENWithMoveNumbers :: FEN
ohcFENWithMoveNumbers = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k - 0 16"

invalidFENBecauseMissingInfo :: FEN
invalidFENBecauseMissingInfo = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4"

invalidFENBecauseNoWhiteKing :: FEN 
invalidFENBecauseNoWhiteKing = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2R4 w k -"

openGameFENWithEnPassantAndMoveNumbers :: FEN
openGameFENWithEnPassantAndMoveNumbers = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"

openGameFEN :: FEN
openGameFEN = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq -"

najdorfFENWithValidEnPassant :: FEN
najdorfFENWithValidEnPassant = "r2q1rk1/3nb1pp/p2p4/1p1PppPn/8/1N2BP2/PPPQ3P/2KR1B1R w - f6"

endgamePuzzle1 :: Puzzle
endgamePuzzle1 = 
  { 
    name: "endgame 1",
    fen: "8/2KP1k2/3Pq3/8/8/8/8/8 w - -",
    move: {
      from: "d7",
      to: "d8",
      underpromotion: Just Knight
    },
    box: 2,
    lastDrilledAt: 1601324525
  }

endgamePuzzle2 :: Puzzle
endgamePuzzle2 = 
  { 
    name: "endgame 2",
    fen: "8/5p2/8/6Pk/5P2/8/8/7K w - -",
    move: {
      from: "g6",
      to: "g7",
      underpromotion: Nothing
    },
    box: 4,
    lastDrilledAt: 1601324534
  }

twoEndgamePuzzles :: Array Puzzle
twoEndgamePuzzles = [endgamePuzzle1, endgamePuzzle2]