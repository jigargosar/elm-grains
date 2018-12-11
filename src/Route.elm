module Route exposing (Route(..), fromString)

import GrainId exposing (GrainId)
import Url
import Url.Parser exposing ((</>), Parser, custom, int, map, oneOf, parse, s, string, top)


type Route
    = GrainList
    | Grain GrainId
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map GrainList top
        , map Grain (s "grain" </> grainId_)
        ]


grainId_ : Parser (GrainId -> a) a
grainId_ =
    custom "GRAIN_ID" <|
        \segment ->
            GrainId.fromString segment


fromString : String -> Route
fromString string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (parse route url)
