module Action exposing (Action, ActionType(..))

import GrainFilter exposing (GrainFilter)


type ActionType
    = SwitchFilter GrainFilter


type alias Action =
    { name : String
    , type_ : ActionType
    }
