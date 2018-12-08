module Msg exposing (Msg(..))

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


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | LogError String
      -- GRAIN
    | GrainGeneratorReceived (Generator Grain)
    | GrainAdd Grain
      -- GRAIN LIST NAVIGATION
    | Prev
    | Next
