module TestData where

import Types (Action(..), Name, FEN, Move, View(..), Alert(..), Puzzle)
import Constants (firstBox)

ohcFEN :: FEN
ohcFEN = "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -"

ohcName :: Name
ohcName = "Opera House Checkmate"

ohcMove :: Move
ohcMove = "b3b8"

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
    move: "d7d8k",
    box: 2,
    lastDrilledAt: 1601324525
  }

endgamePuzzle2 :: Puzzle
endgamePuzzle2 = 
  { 
    name: "endgame 2",
    fen: "8/5p2/8/6Pk/5P2/8/8/7K w - -",
    move: "g6g7",
    box: 4,
    lastDrilledAt: 1601324534
  }

ohcPuzzle :: Puzzle
ohcPuzzle = 
  {
    name: ohcName,
    fen: ohcFEN,
    move: ohcMove,
    box: firstBox,
    lastDrilledAt: 0
  }

openGamePuzzle :: Puzzle
openGamePuzzle = 
  {
    name: "Open Game",
    fen: openGameFEN,
    move: "g1f3",
    box: firstBox,
    lastDrilledAt: 0
  }

twoEndgamePuzzles :: Array Puzzle
twoEndgamePuzzles = [endgamePuzzle1, endgamePuzzle2]

twoEndgamePuzzlesJSON :: String
twoEndgamePuzzlesJSON = """
  [
    { 
      name: "endgame 1",
      fen: "8/2KP1k2/3Pq3/8/8/8/8/8 w - -",
      move: "d7d8k",
      box: 2,
      lastDrilledAt: 1601324525
    },
    { 
      name: "endgame 2",
      fen: "8/5p2/8/6Pk/5P2/8/8/7K w - -",
      move: "g6g7",
      box: 4,
      lastDrilledAt: 1601324534
    }
  ]
"""