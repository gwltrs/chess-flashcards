module Test.Values where
  
import Prelude

import Types

initialState :: State
initialState = { puzzles: [], reviewStack: [], view: LoadingFile }

newFileState :: State
newFileState = { puzzles: [], reviewStack: [], view: MainMenu "" "" }