module Grain exposing (Grain)

import Tagged exposing (Tagged(..), tag)


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


type alias Grain =
    Tagged GrainTag Internal


mockInit : String -> Grain
mockInit title =
    tag { id = tag title, title = title }


mockList : List Grain
mockList =
    [ "Line 1"
    , "Line 2"
    ]
        |> List.map mockInit
