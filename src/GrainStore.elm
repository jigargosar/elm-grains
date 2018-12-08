
module GrainStore exposing (GrainStore)

import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)


type alias Model =
    { lookup : Dict GrainId Grain }


type GrainStore
    = GrainStore Model
