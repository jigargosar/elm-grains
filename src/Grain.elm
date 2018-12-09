module Grain exposing (Grain)

import Tagged exposing (Tagged(..), tag)


type GrainIdTag
    = GrainIdTag


type GrainTag
    = GrainTag


type alias Internal =
    { id : Tagged GrainIdTag String
    }


type alias Grain =
    Tagged GrainTag Internal
