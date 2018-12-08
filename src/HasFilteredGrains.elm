module HasFilteredGrains exposing
    ( HasFilteredGrains
    , getFilterPred
    , selectedGrain
    )

import BasicsX exposing (callWith)
import Grain
import GrainFilter exposing (GrainFilter)
import GrainList exposing (GrainList)


type alias HasFilteredGrains x =
    { x
        | grains : GrainList
        , filter : GrainFilter
    }


getFilterPred =
    .filter >> GrainFilter.toPred


selectedGrain hasFG =
    hasFG.grains |> GrainList.filteredSelected (getFilterPred hasFG)
