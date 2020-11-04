module RenderTests where

import Prelude (Unit, discard, (<>))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Types (Action(..), Alert(..), View(..), FirstAttempt(..))
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..))

import Render (nextButtonIsEnabled)
import TestData

renderTests :: Free TestF Unit
renderTests = suite "Render" do

  test "Next button is enabled after user makes correct move" do

    Assert.equal 
      true
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just true), 
          alert: Nothing
        }
      )

    Assert.equal 
      true
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just ohcMove) (Just false), 
          alert: Nothing
        }
      )

  test "Next button is disabled after user makes incorrect move" do

    Assert.equal 
      false
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just "a1h8") (Just true), 
          alert: Nothing
        }
      )

    Assert.equal 
      false
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN (Just "a1h8") (Just false), 
          alert: Nothing
        }
      )

  test "Next button is disabled before before move has been made" do

    -- User isn't allowed to go to the next puzzle if a move still needs to be
    -- made even if they've already gotten the answer correct

    Assert.equal 
      false
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing Nothing, 
          alert: Nothing
        }
      )

    Assert.equal 
      false
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just true), 
          alert: Nothing
        }
      )

    Assert.equal 
      false
      (nextButtonIsEnabled 
        { 
          puzzles: [openGamePuzzle, ohcPuzzle], 
          reviewStack: ["Open Game"], 
          view: ReviewingPuzzle ohcName ohcFEN Nothing (Just false), 
          alert: Nothing
        }
      )