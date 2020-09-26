module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Types
import Reducer (reducer)
import Render

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { 
      initialState, 
      render, 
      eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { puzzles: [], reviewStack: [], view: LoadingFile }

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction action =
  H.modify_ \state -> reducer state action