module ListIndex exposing
    ( ListIndex
    , empty
    , initFromList
    , rollBy
    , selectBy
    , selected
    , selectedMap
    )

import BasicsX exposing (atClampedIdx, clampIdxIn, rollIdx, unwrapMaybe)
import List.Extra as List


type alias Model =
    { idx : Int
    }


type ListIndex
    = ListIndex Model


empty =
    ListIndex { idx = 0 }


initFromList list =
    ListIndex { idx = 0 }


unwrap : ListIndex -> Model
unwrap (ListIndex model) =
    model


idx =
    unwrap >> .idx


map fn =
    unwrap >> fn >> ListIndex


mapIdx fn =
    map (\model -> { model | idx = fn model.idx })


selectedMap fn list listIndex =
    let
        ims si =
            List.indexedMap (\i -> fn { i = i, si = si, selected = i == si })
                list
    in
    idx listIndex
        |> BasicsX.clampIdxIn list
        |> unwrapMaybe [] ims


rollBy offset list listIndex =
    idx listIndex
        |> clampIdxIn list
        |> Maybe.andThen ((+) offset >> rollIdx list)
        |> unwrapMaybe listIndex (\ii -> ListIndex { idx = ii })


selected list listIndex =
    idx listIndex |> atClampedIdx list


selectBy pred list =
    mapIdx (\ii -> List.findIndex pred list |> Maybe.withDefault ii)
