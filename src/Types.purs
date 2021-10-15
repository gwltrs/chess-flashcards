module Types where

import Data.Maybe (Maybe)
import Data.Eq (class Eq)
import Data.Show (class Show, show)
import Data.Semigroup ((<>))

type State = 
  { 
    puzzles :: Array Puzzle, 
    reviewStack :: Array Name, -- Names of the puzzles that are up for review
    view :: View,
    alert :: Maybe Alert
  }

type Puzzle = 
  { 
    name :: Name,
    fen :: FEN,
    move :: Move,
    box :: Days,
    lastDrilledAt :: TimestampSeconds
  }

type Name = String

type FEN = String

-- In the format: fromSpace <> toSpace <> underpromotionFirstLetter
-- Examples: 'c4d5', 'c7c8r', 'h2h1b', 'f7f8k'
type Move = String 

type Days = Int

type TimestampSeconds = Int

data View = 
  LoadingFile | 
  MainMenu Name FEN | 
  CreatingPuzzle Name FEN (Maybe Move) |
  ReviewingPuzzle Name FEN (Maybe Move) FirstAttempt

-- Represents the result of the user's first attempt of the puzzle.
-- Nothing -> Not attempted yet.
-- Just true -> Correct first attempt.
-- Just false -> Incorrect first attempt.
type FirstAttempt = Maybe Boolean

derive instance eqView :: Eq View

instance showView :: Show View where
  show = case _ of
    LoadingFile -> "LoadingFile"
    MainMenu puzzleName fen -> "MainMenu " <> show puzzleName <> " " <> show fen
    CreatingPuzzle puzzleName fen move -> "CreatingPuzzle " <> show puzzleName <> " " <> show fen <> " " <> show move
    ReviewingPuzzle puzzleName fen move firstAttempt -> "ReviewingPuzzle " <> show puzzleName <> " " <> show fen <> " " <> show move <> show firstAttempt

data Alert = 
  MissingNameOrFEN |
  InvalidFEN | 
  DuplicateFEN |
  InvalidFile |
  NoPuzzlesForReview |
  AllPuzzlesReviewed |
  ThisIsTheName Name

derive instance eqAlert :: Eq Alert

instance showAlert :: Show Alert where
  show alert = case alert of
    MissingNameOrFEN -> "MissingNameOrFEN"
    InvalidFEN -> "InvalidFEN"
    DuplicateFEN -> "DuplicateFEN"
    InvalidFile -> "InvalidFile"
    NoPuzzlesForReview -> "NoPuzzlesForReview"
    AllPuzzlesReviewed -> "AllPuzzlesReviewed"
    ThisIsTheName name -> "ThisIsTheName" <> " " <> name

data Action = 
  NewFile | 
  OpenFileDialog |
  LoadFile String TimestampSeconds | 
  SaveFile | 
  Review | 
  AttemptPuzzle Move TimestampSeconds |
  UpdatePuzzleName Name |
  UpdateFEN FEN |
  CreatePuzzle |
  AddMoveToNewPuzzle Move |
  StartSavingPuzzle |
  SavePuzzle TimestampSeconds |
  BackToMain | 
  Retry | 
  ShowName | 
  CopyFEN