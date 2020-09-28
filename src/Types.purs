module Types where

import Data.Maybe (Maybe)
import Data.Eq (class Eq)
import Data.Show (class Show, show)
import Data.Semigroup ((<>))

type State = 
  { 
    puzzles :: Array Puzzle, 
    reviewStack :: Array PuzzleName, -- Names of the puzzles that are up for review
    view :: View,
    alert :: Maybe Alert
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
  CreatingPuzzle PuzzleName FEN (Maybe Move)

derive instance eqView :: Eq View

instance showView :: Show View where
  show view = case view of
    LoadingFile -> "LoadingFile"
    MainMenu puzzleName fen -> "MainMenu " <> show puzzleName <> " " <> show fen
    CreatingPuzzle puzzleName fen move -> "CreatingPuzzle " <> show puzzleName <> " " <> show fen <> " " <> show move

data Alert = 
  MissingNameOrFEN |
  InvalidFEN

derive instance eqAlert :: Eq Alert

instance showAlert :: Show Alert where
  show alert = case alert of
    MissingNameOrFEN -> "MissingNameOrFEN"
    InvalidFEN -> "InvalidFEN"

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