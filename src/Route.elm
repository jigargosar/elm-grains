module Route exposing (Route(..))

import GrainId exposing (GrainId)


type Route
    = GrainList
    | Grain GrainId
