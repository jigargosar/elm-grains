module Msg exposing (GM(..), Msg(..))

import Action exposing (Action)
import Browser.Dom
import Grain exposing (Grain)
import GrainFilter exposing (GrainFilter)
import GrainId exposing (GrainId)
import Label exposing (Label)
import LabelId exposing (LabelId)
import LabelLayerItem exposing (LabelLayerItem)
import QueryPrefix
import Random exposing (Generator)


type GM
    = GMNew
    | GMOnGen (Generator Grain)
    | GMAdd Grain
    | GMTitle GrainId String


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | LogError String
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | InputChanged String
    | InputSubmit
      -- GRAIN
    | SubGM GM
      -- GRAIN LIST NAVIGATION
    | Prev
    | Next
