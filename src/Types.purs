module Types where

import Data.Maybe (Maybe)
import Data.Eq (class Eq)
import Data.Show (class Show, show)
import Data.Semigroup ((<>))
--import Data.Ord (class Ord, Ordering)

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

-- When a puzzle is attempted, random variance is subtracted from the puzzle's newly-calculated 
-- last-drilled-at timestamp. The intention is to avoid long-term temporal association between 
-- puzzles. For example, if I enter 10 checkmate positions into the app in one day, in the future 
-- those 10 positions will be reviewed on the same day in the same order indefinitely. This will 
-- continue to happen until some of the 10 are missed in reviews. We avoid this review-mode 
-- predictability by subtly shifting the timestamp randomly within a range of values. The
-- timestamp is adjusted: last_drilled_at = last_drilled_at - (round (variance_factor * box_after_attempt * seconds_in_a_day)).
type VarianceFactor = Number

data View = 
  LoadingFile | 
  MainMenu Name FEN | 
  CreatingPuzzle Name FEN (Maybe Move) |
  ReviewingPuzzle Name FEN (Maybe Move) IsFirstAttempt

type IsFirstAttempt = Boolean

derive instance eqView :: Eq View

instance showView :: Show View where
  show view = case view of
    LoadingFile -> "LoadingFile"
    MainMenu puzzleName fen -> "MainMenu " <> show puzzleName <> " " <> show fen
    CreatingPuzzle puzzleName fen move -> "CreatingPuzzle " <> show puzzleName <> " " <> show fen <> " " <> show move
    ReviewingPuzzle puzzleName fen move isFirstAttempt -> "ReviewingPuzzle " <> show puzzleName <> " " <> show fen <> " " <> show move <> show isFirstAttempt

data Alert = 
  MissingNameOrFEN |
  InvalidFEN | 
  DuplicateFEN |
  InvalidFile |
  NoPuzzlesForReview |
  AllPuzzlesReviewed

derive instance eqAlert :: Eq Alert

instance showAlert :: Show Alert where
  show alert = case alert of
    MissingNameOrFEN -> "MissingNameOrFEN"
    InvalidFEN -> "InvalidFEN"
    DuplicateFEN -> "DuplicateFEN"
    InvalidFile -> "InvalidFile"
    NoPuzzlesForReview -> "NoPuzzlesForReview"
    AllPuzzlesReviewed -> "AllPuzzlesReviewed"

data Action = 
  NewFile | 
  OpenFileDialog |
  LoadFile String TimestampSeconds | 
  SaveFile | 
  Review | 
  AttemptPuzzle Move TimestampSeconds VarianceFactor |
  UpdatePuzzleName Name |
  UpdateFEN FEN |
  CreatePuzzle |
  AddMoveToNewPuzzle Move |
  SavePuzzle |
  BackToMain | 
  Retry | 
  ShowTitle | 
  CopyFEN