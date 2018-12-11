module Grain exposing
    ( Grain
    , generator
    , title
    , toDomIdWithPrefix
    )

import GrainId exposing (GrainId(..))
import Random exposing (Generator)


type alias Model =
    { id : GrainId
    , title : String
    }


type Grain
    = Grain Model


init : GrainId -> Grain
init initialId =
    Grain { id = initialId, title = "" }


unwrap (Grain model) =
    model


map fn =
    unwrap >> fn >> Grain


title =
    unwrap >> .title


id =
    unwrap >> .id


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


generator : Generator Grain
generator =
    GrainId.generator |> Random.map init
