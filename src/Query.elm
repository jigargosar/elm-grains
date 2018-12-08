module Query exposing
    ( Query
    , init
    , inputFieldValue
    , matchesString
    , modifyWithPrefixedInputValue
    , prefix
    )

import BasicsX exposing (callWith, when)
import QueryPrefix exposing (QueryPrefix)


type alias Model =
    { value : String
    , prefix : QueryPrefix
    }


type Query
    = Query Model


init qp =
    Query { value = "", prefix = qp }


unwrap : Query -> Model
unwrap (Query model) =
    model


prefix =
    unwrap >> .prefix


inputFieldValue query =
    QueryPrefix.asString (prefix query) ++ value query


value =
    unwrap >> .value


modify fn =
    unwrap >> fn >> Query


modifyWithPrefixedInputValue inputString =
    let
        ( newPrefix, newValue ) =
            QueryPrefix.unConsFromString inputString
    in
    modify (\model -> { model | value = newValue, prefix = newPrefix })


boiledValue =
    value >> boilString


boilString =
    String.trim >> String.toLower


matchesString hay query =
    String.contains (boiledValue query) (boilString hay)
