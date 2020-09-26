module Types where

import Data.Maybe (Maybe(..))
import Data.Eq
import Data.Show
import Data.Semigroup

type State = 
  { 
    puzzles :: Array Puzzle, 
    reviewStack :: Array PuzzleName, -- Names of the puzzles that are up for review
    view :: View
  }

type Puzzle = 
  { 
    name :: PuzzleName,
    fen :: FEN,
    move :: Move,
    box :: Int,
    lastDrilledAt :: Int
  }

type PuzzleName = String

type FEN = String

type Move = 
  { 
    from :: Space,
    to :: Space,
    underpromotion :: Maybe Underpromotion
  }

type Space = String -- In the format of 'a8', 'h4', 'f3', etc.

data Underpromotion = 
  Bishop | 
  Knight | 
  Rook

derive instance eqUnderpromotion :: Eq Underpromotion

instance showUnderpromotion :: Show Underpromotion where
  show up = case up of
    Bishop -> "Bishop"
    Knight -> "Knight"
    Rook -> "Rook"

data View = 
  LoadingFile | 
  MainMenu PuzzleName FEN | 
  CreatingPuzzle

derive instance eqView :: Eq View

instance showView :: Show View where
  show view = case view of
    LoadingFile -> "LoadingFile"
    MainMenu puzzleName fen -> "MainMenu " <> show puzzleName <> " " <> show fen
    CreatingPuzzle -> "CreatingPuzzle"

data Action = 
  NewFile | 
  LoadFile | 
  SaveFile | 
  Review | 
  UpdatePuzzleName PuzzleName |
  UpdateFEN FEN |
  CreatePuzzle | 
  BackToMain |
  SavePuzzle