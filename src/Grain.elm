module Grain exposing (Grain, generator, title)

import GrainId exposing (GrainId(..))
import Random exposing (Generator)


type alias Model =
    { id : GrainId
    , title : String
    }


type Grain
    = Grain Model


init : GrainId -> Grain
init id =
    Grain { id = id, title = "" }


unwrap (Grain model) =
    model


map fn =
    unwrap >> fn >> Grain


title =
    unwrap >> .title


generator : Generator Grain
generator =
    GrainId.generator |> Random.map init
