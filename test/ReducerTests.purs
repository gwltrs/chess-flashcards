module ReducerTests where

import Prelude (Unit, discard, (<>))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Types (Action(..), Alert(..), View(..))
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..))

import Reducer (reducer)
import TestData

reducerTests :: Free TestF Unit
reducerTests = suite "Reducer" do

  test "User creates New file" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing } 
        NewFile
      ) 

  test "User types in the Name field" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "", alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
        (UpdatePuzzleName "Opera Hou")
      ) 
        
    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" "", alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera Hou" "", alert: Nothing } 
        (UpdatePuzzleName "Opera House Checkmate")
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" ohcFEN, alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "Opera House Checkmate" ohcFEN, alert: Nothing } 
        (UpdatePuzzleName "OHC")
      ) 

  test "User types in the FEN field" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
        (UpdateFEN ohcFEN)
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" "not real FEN", alert: Nothing }
        (reducer 
          { puzzles: [], reviewStack: [], view: MainMenu "OHC" ohcFEN, alert: Nothing } 
          (UpdateFEN "not real FEN")
        ) 

  test "User tries to create puzzle with empty field(s)" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Just MissingNameOrFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing } 
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Just MissingNameOrFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "" ohcFEN, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" "", alert: Just MissingNameOrFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "", alert: Nothing } 
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu " " ohcFEN, alert: Just MissingNameOrFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu " " ohcFEN, alert: Nothing } -- Should check for length after trimming
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" "   ", alert: Just MissingNameOrFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" "   ", alert: Nothing } -- Should check for length after trimming
        CreatePuzzle
      ) 

  test "User tries to create puzzle with invalid FEN" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseMissingInfo, alert: Just InvalidFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseMissingInfo, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseNoWhiteKing, alert: Just InvalidFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" invalidFENBecauseNoWhiteKing, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "complete garbage" "asdlfkj9p34@#$%WEGWG", alert: Just InvalidFEN }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "complete garbage" "asdlfkj9p34@#$%WEGWG", alert: Nothing }
        CreatePuzzle
      ) 

  test "User tries to create puzzle with duplicate fen" do

    -- Check should happen after FEN is trimmed and sanitized (move numbers and unnecessary en passant removed)

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" endgamePuzzle1.fen, alert: Just DuplicateFEN }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" endgamePuzzle1.fen, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" (" " <> endgamePuzzle2.fen <> " "), alert: Just DuplicateFEN }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "new endgame" (" " <> endgamePuzzle2.fen <> " "), alert: Nothing }
        CreatePuzzle
      )

    Assert.equal 
      { puzzles: [openGamePuzzle], reviewStack: [], view: MainMenu "develop" (" " <> openGameFENWithEnPassantAndMoveNumbers), alert: Just DuplicateFEN }
      (reducer 
        { puzzles: [openGamePuzzle], reviewStack: [], view: MainMenu "develop" (" " <> openGameFENWithEnPassantAndMoveNumbers), alert: Nothing }
        CreatePuzzle
      )

  test "User navigates to create-puzzle view" do

    -- Both the puzzle name and FEN should be trimmed
    -- Move numbers from FEN should be stripped
    -- Unnecessary en passant info in FEN should be replaced with "-"
    -- Puzzle name "auto-increment" feature
    --   This allows the user to name the puzzle without having to worry about conflicting names
    --   "checkmate pattern" -> "checkmate pattern" if no puzzles already exist
    --   "endgame" -> "endgame #3" if two puzzles already exist with the names "endgame" and "endgame #2"

    Assert.equal
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "OHC" ohcFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "OHC" (" " <> ohcFEN <> "  "), alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: CreatingPuzzle "OHC" ohcFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "OHC" (" " <> ohcFENWithMoveNumbers), alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "Open Game" openGameFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "   Open Game " openGameFENWithEnPassantAndMoveNumbers, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [], reviewStack: [], view: CreatingPuzzle "Open Game" openGameFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "   Open Game " openGameFENWithEnPassantAndMoveNumbers, alert: Nothing }
        CreatePuzzle
      ) 

    Assert.equal 
      { puzzles: [openGamePuzzle], reviewStack: [], view: CreatingPuzzle "Open Game #2" ohcFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: [openGamePuzzle], reviewStack: [], view: MainMenu "   Open Game " ohcFEN, alert: Nothing }
        CreatePuzzle
      )

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle "endgame #3" najdorfFENWithValidEnPassant Nothing, alert: Nothing }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "endgame" najdorfFENWithValidEnPassant, alert: Nothing }
        CreatePuzzle
      )

    Assert.equal 
      { puzzles: [openGamePuzzle, largeIncrementPuzzle], reviewStack: [], view: CreatingPuzzle "Open Game #2" ohcFEN Nothing, alert: Nothing }
      (reducer 
        { puzzles: [openGamePuzzle, largeIncrementPuzzle], reviewStack: [], view: MainMenu "   Open Game " ohcFEN, alert: Nothing }
        CreatePuzzle
      )

    Assert.equal 
      { puzzles: twoEndgamePuzzles <> [largeIncrementPuzzle], reviewStack: [], view: CreatingPuzzle "endgame #3" najdorfFENWithValidEnPassant Nothing, alert: Nothing }
      (reducer 
        { puzzles: twoEndgamePuzzles <> [largeIncrementPuzzle], reviewStack: [], view: MainMenu "endgame" najdorfFENWithValidEnPassant, alert: Nothing }
        CreatePuzzle
      )

  test "User adds move to new puzzle" do

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle ohcName ohcFEN (Just ohcMove), alert: Nothing }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: CreatingPuzzle ohcName ohcFEN Nothing, alert: Nothing }
        (AddMoveToNewPuzzle ohcMove)
      )

  test "User saves puzzle after adding a move" do

    -- Puzzles should be sorted by name ascending.
    -- Not testing use case where the move hasn't been made since the 
    -- save button is disabled when the move hasn't been made yet.
    -- Should return user to main menu after saving puzzle.
    -- Saved puzzle should have the current timestamp

    Assert.equal 
      { puzzles: [ohcPuzzle { lastDrilledAt = 1_500_000 }], reviewStack: [], view: MainMenu "" "", alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: CreatingPuzzle ohcName ohcFEN (Just ohcMove), alert: Nothing }
        (SavePuzzle 1_500_000)
      )

    Assert.equal 
      { puzzles: [ohcPuzzle { name = "a", lastDrilledAt = 1_000_000 }, endgamePuzzle1, endgamePuzzle2], reviewStack: [], view: MainMenu "" "", alert: Nothing }
      (reducer 
        { puzzles: [endgamePuzzle1, endgamePuzzle2], reviewStack: [], view: CreatingPuzzle "a" ohcFEN (Just ohcMove), alert: Nothing }
        (SavePuzzle 1_000_000)
      )

    Assert.equal 
      { puzzles: [endgamePuzzle1, endgamePuzzle2, ohcPuzzle { name = "z", lastDrilledAt = 123456789 }], reviewStack: [], view: MainMenu "" "", alert: Nothing }
      (reducer 
        { puzzles: [endgamePuzzle1, endgamePuzzle2], reviewStack: [], view: CreatingPuzzle "z" ohcFEN (Just ohcMove), alert: Nothing }
        (SavePuzzle 123456789)
      )

  test "User tries to load an invalid file" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: LoadingFile, alert: Just InvalidFile }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }
        (LoadFile "" 9001)
      )
    
    Assert.equal 
      { puzzles: [], reviewStack: [], view: LoadingFile, alert: Just InvalidFile }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }
        (LoadFile "{}" 9001)
      )

    Assert.equal 
      { puzzles: [], reviewStack: [], view: LoadingFile, alert: Just InvalidFile }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }
        (LoadFile "[{'nme':'asdf','fen':'asdfsf','move':-123,'box':-1,'laaasssstDrilledAt':false}]" 12341234)
      )

  test "User loads a valid file" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "" "", alert: Nothing }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }
        (LoadFile "[]" 9001)
      )

    -- The following test(s) not only assert the JSON-parsing logic but also the generation of the review stack.
    -- The review stack is only generated when the file is loaded.
    -- The review stack should be sorted descending based on how "overdue" the puzzle is for review
    -- Puzzles are added to the review stack if: currentTimestamp > (puzzle.lastDrilledAt) 

    Assert.equal 
      { 
        puzzles: fourAssortedTimestampPuzzles, 
        reviewStack: fourAssortedTimestampPuzzlesExpectedReviewStack, 
        view: MainMenu "" "", 
        alert: Nothing 
      }
      (reducer 
        { puzzles: [], reviewStack: [], view: LoadingFile, alert: Nothing }
        (LoadFile fourAssortedTimestampPuzzlesJSON 1_000_000_000)
      )

  test "User tries to review but no puzzles are up for review" do

    Assert.equal 
      { puzzles: [], reviewStack: [], view: MainMenu "asdf" "", alert: Just NoPuzzlesForReview }
      (reducer 
        { puzzles: [], reviewStack: [], view: MainMenu "asdf" "", alert: Nothing }
        Review
      )

    Assert.equal 
      { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "" "", alert: Just NoPuzzlesForReview }
      (reducer 
        { puzzles: twoEndgamePuzzles, reviewStack: [], view: MainMenu "" "", alert: Nothing }
        Review
      )
  
  test "User starts reviewing puzzles" do

    -- Should simply grab the title of the puzzle at review stack index 0.
    -- Puzzles aren't removed from the review stack until they are attempted.

    Assert.equal 
      { puzzles: [openGamePuzzle], reviewStack: ["Open Game"], view: ReviewingPuzzle "Open Game" openGameFEN Nothing Nothing, alert: Nothing }
      (reducer 
        { puzzles: [openGamePuzzle], reviewStack: ["Open Game"], view: MainMenu "a" "b", alert: Nothing }
        Review
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: [ohcName, "Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN Nothing Nothing, 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: MainMenu "" "", 
          alert: Nothing
        }
        Review
      )

  test "User continues reviewing puzzles" do

    -- Should simply grab the title of the puzzle at review stack index 0.
    -- Puzzles aren't removed from the review stack until they are attempted.

    -- Not testing use case where the move hasn't been made since the 
    -- save button is disabled when the move hasn't been made yet.
    -- Also not testing use case where the user gets the puzzle wrong and then hits
    -- Next since the button will also be disabled. (This is being tested in RenderTests).

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle "Open Game" openGameFEN Nothing Nothing, 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
          alert: Nothing
        }
        Review
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
        reviewStack: ["Open Game", endgamePuzzle1.name], 
        view: ReviewingPuzzle "Open Game" openGameFEN Nothing Nothing, 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
          reviewStack: ["Open Game", endgamePuzzle1.name], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
          alert: Nothing
        }
        Review
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
        reviewStack: ["Open Game", endgamePuzzle1.name], 
        view: ReviewingPuzzle "Open Game" openGameFEN Nothing Nothing, 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
          reviewStack: ["Open Game", endgamePuzzle1.name], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just false), 
          alert: Nothing
        }
        Review
      )

  test "User finishes reviewing all the puzzles" do

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
        reviewStack: [], 
        view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
        alert: Just AllPuzzlesReviewed
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
          reviewStack: [], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
          alert: Nothing
        }
        Review
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
        reviewStack: [], 
        view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just false), 
        alert: Just AllPuzzlesReviewed
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle, endgamePuzzle1], 
          reviewStack: [], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just false), 
          alert: Nothing
        }
        Review
      )

  test "User makes attempts after the first attempt" do

    -- Puzzles shouldn't be updated for answers (correct or incorrect) after the first attempt.
    -- The first attempt is for evaluation. Subsequent attempts are enabled for reinforcement 
    -- on puzzles that have left the user's memory to some degree.

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: [ohcName, "Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just true), 
          alert: Nothing
        }
        (AttemptPuzzle ohcMove 1601324534 0.5) -- Correct
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: [ohcName, "Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just false), 
          alert: Nothing
        }
        (AttemptPuzzle ohcMove 1601324534 0.5) -- Correct
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: [ohcName, "Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just true), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just true), 
          alert: Nothing
        }
        (AttemptPuzzle "d1d7" 1601324534 0.5) -- Incorrect
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: [ohcName, "Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just false), 
          alert: Nothing
        }
        (AttemptPuzzle "d1d7" 1601324534 0.5) -- Incorrect
      )

  test "User makes wrong move on the first attempt" do

    Assert.equal 
      { 
        -- The new timestamp is "now" since the variance factor is 0
        puzzles: [openGamePuzzle, ohcPuzzle { box = 1, lastDrilledAt = 1_000_000_000 }], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing Nothing, 
          alert: Nothing
        }
        (AttemptPuzzle "d1d7" 1_000_000_000 0.0)
      )

    Assert.equal 
      { 
        -- The new timestamp is equal to 2_000_000_000 - seconds-in-half-of-a-day
        puzzles: [endgamePuzzle1 { box = 1, lastDrilledAt = 1_999_956_800} , endgamePuzzle2], 
        reviewStack: [endgamePuzzle2.name], 
        view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen (Just "a1h8") (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: twoEndgamePuzzles, 
          reviewStack: [endgamePuzzle1.name, endgamePuzzle2.name], 
          view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen Nothing Nothing, 
          alert: Nothing
        }
        (AttemptPuzzle "a1h8" 2_000_000_000 0.5)
      )

  test "User makes correct move on the first attempt" do

    Assert.equal 
      { 
        -- The new timestamp is equal to 1_000_000_000 - seconds-in-a-day (since the new box is 4 and the variance factor is 1/4)
        puzzles: [endgamePuzzle1 { box = 4, lastDrilledAt = 999_913_600}, endgamePuzzle2], 
        reviewStack: [endgamePuzzle2.name], 
        view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen (Just endgamePuzzle1.move) (Just true), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: twoEndgamePuzzles, 
          reviewStack: [endgamePuzzle1.name, endgamePuzzle2.name], 
          view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen Nothing Nothing, 
          alert: Nothing
        }
        (AttemptPuzzle endgamePuzzle1.move 1_000_000_000 0.25)
      )

    Assert.equal 
      { 
        -- The new timestamp is "now" since the variance factor is 0.
        -- New box should be capped at 64.
        puzzles: [openGamePuzzle, ohcPuzzle { box = 64, lastDrilledAt = 1_999_447_040 }], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle { box = 40 }], 
          reviewStack: [ohcName, "Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing Nothing, 
          alert: Nothing
        }
        (AttemptPuzzle ohcMove 2_000_000_000 0.1)
      )

  test "User retries a puzzle" do

    -- Should simply replace the Just move with Nothing

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle { box = 1, lastDrilledAt = 1_000_000_000 }], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN Nothing (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle { box = 1, lastDrilledAt = 1_000_000_000 }], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just false), 
          alert: Nothing
        }
        Retry
      )

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle { box = 1, lastDrilledAt = 1_000_000_000 }], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN Nothing (Just true), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle { box = 1, lastDrilledAt = 1_000_000_000 }], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
          alert: Nothing
        }
        Retry
      )

    Assert.equal 
      { 
        puzzles: [endgamePuzzle1 { box = 4, lastDrilledAt = 999_913_600 }, endgamePuzzle2], 
        reviewStack: [endgamePuzzle2.name], 
        view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen Nothing (Just false), 
        alert: Nothing
      }
      (reducer 
        { 
          puzzles: [endgamePuzzle1 { box = 4, lastDrilledAt = 999_913_600 }, endgamePuzzle2], 
          reviewStack: [endgamePuzzle2.name], 
          view: ReviewingPuzzle endgamePuzzle1.name endgamePuzzle1.fen (Just endgamePuzzle1.move) (Just false), 
          alert: Nothing
        }
        Retry
      )

  test "User reveals puzzle name" do

    -- Should simply add the ThisIsTheName alert

    Assert.equal 
      { 
        puzzles: [openGamePuzzle, ohcPuzzle], 
        reviewStack: ["Open Game"], 
        view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just false), 
        alert: Just (ThisIsTheName ohcName)
      }
      (reducer 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just "d1d7") (Just false), 
          alert: Nothing
        }
        ShowName
      )