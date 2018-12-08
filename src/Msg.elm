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
    = GMNew String
    | GMOnGen (Generator Grain)
    | GMAdd Grain


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | LogError String
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
      -- GRAIN
    | SubGM GM
      -- GRAIN LIST NAVIGATION
    | Prev
    | Next
