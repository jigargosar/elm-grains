module QueryPrefix exposing
    ( QueryPrefix(..)
    , asString
    , unConsFromString
    )

import String.Extra as String


type QueryPrefix
    = None
    | Label
    | GrainFilter
    | Bucket


asString qp =
    case qp of
        None ->
            ""

        Label ->
            "#"

        GrainFilter ->
            "?"

        Bucket ->
            "@"


unConsFromString s =
    let
        trimmed =
            String.trimLeft s

        withoutFirstChar =
            trimmed |> String.dropLeft 1
    in
    case trimmed |> String.left 1 of
        "#" ->
            ( Label, withoutFirstChar )

        "?" ->
            ( GrainFilter, withoutFirstChar )

        "@" ->
            ( Bucket, withoutFirstChar )

        _ ->
            ( None, s )
