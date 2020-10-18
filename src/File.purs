module File where

import Prelude
import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Halogen.HTML.Core as HC
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed.InputType (InputType(..))
  
-- First parameter is the name of the file (including the extension)
-- Second parameter is the contents of the file
foreign import saveFile :: String -> String -> Effect Unit

foreign import openFileDialogImpl :: Int -> Effect (Promise String)

-- getMove :: FEN -> Move -> Aff Move
-- getMove fen expectedMove = toAffE (getMoveImpl fen expectedMove) -- Try to make this point-free later

openFileDialog :: Int -> Aff String
openFileDialog x = toAffE (openFileDialogImpl x)

-- <input id="fileInput" type="file" name="name" style="display: none;"/>
openFileDialogInput :: forall w i. HC.HTML i w
openFileDialogInput = 
  HH.input [
    HP.id_ "fileInput",
    HP.name "name",
    HP.type_ InputFile
  ]