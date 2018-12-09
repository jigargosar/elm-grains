module Grain exposing (Grain, mockList, title)

import Tagged exposing (Tagged(..), tag, untag)


type GrainIdTag
    = GrainIdTag


type alias GrainId =
    Tagged GrainTag String


type GrainTag
    = GrainTag


type alias Internal =
    { id : GrainId
    , title : String
    }


title =
    untag >> .title


type alias Grain =
    Tagged GrainTag Internal


mockInit : String -> Grain
mockInit newTitle =
    tag { id = tag newTitle, title = newTitle }


mockList : List Grain
mockList =
    [ "Line 1"
    , "Line 2"
    ]
        |> List.map mockInit
