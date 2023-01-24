module File where

import Prelude
import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Halogen.HTML.Core as HC
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen.HTML.CSS (style)
import CSS.Display (display, displayNone)
  
-- First parameter is the name of the file (including the extension)
-- Second parameter is the contents of the file
foreign import saveFile :: String -> String -> Effect Unit

-- The openFileDialogInput HTML element must be rendered before openFileDialog is called
openFileDialog :: Aff String
openFileDialog = toAffE openFileDialogImpl

-- We need this "impl" function since it's easiest to 
-- import as Effect (Promise _) and then convert it to an Aff.
foreign import openFileDialogImpl :: Effect (Promise String)

-- Necessary for using open file dialog via openFileDialog.
-- This element is set to "display: none" so it can always be rendered.
openFileDialogInput :: forall w i. HC.HTML i w
openFileDialogInput = 
  HH.input [
    HP.id "fileInput",
    HP.name "name",
    HP.type_ InputFile,
    style do
      display displayNone
  ]