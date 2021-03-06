module Route exposing
    ( Route(..)
    , fromString
    , toString
    )

import GrainId exposing (GrainId)
import Url
import Url.Parser exposing ((</>), Parser, custom, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound String
    | GrainTree GrainId


route : Parser (Route -> a) a
route =
    oneOf
        [ map (GrainTree GrainId.root) top
        , map GrainTree (s "grain" </> grainId_)
        ]


grainId_ : Parser (GrainId -> a) a
grainId_ =
    custom "GRAIN_ID" <|
        \segment ->
            GrainId.fromString segment


fromString : String -> Route
fromString string =
    let
        defaultRoute =
            NotFound string
    in
    case Url.fromString string of
        Nothing ->
            defaultRoute

        Just url ->
            Maybe.withDefault defaultRoute (parse route url)


toString r =
    case r of
        GrainTree gid ->
            if gid == GrainId.root then
                "/"

            else
                "/grain/" ++ GrainId.toString gid

        NotFound string ->
            string
