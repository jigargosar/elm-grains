module RandomId exposing
    ( generator
    , isValidWithPrefix
    )

import BasicsX exposing (eqs)
import Random


idChars : List Char
idChars =
    let
        alphabets =
            { lowercase = "abcdefghijklmnopqrstuvwxyz"
            , uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            }

        numbers =
            "0123456789"
    in
    alphabets.lowercase ++ alphabets.uppercase ++ numbers ++ "_" |> String.toList


idCharGenerator : Random.Generator Char
idCharGenerator =
    Random.uniform '~' idChars


defaultLength =
    21


defaultLengthWithPrefix prefix =
    String.length prefix + defaultLength


isValidWithPrefix prefix =
    let
        expectedLength =
            defaultLengthWithPrefix prefix
    in
    BasicsX.allPass
        [ String.length >> eqs expectedLength
        , String.startsWith prefix
        ]


stringIdGenerator : Random.Generator String
stringIdGenerator =
    Random.list defaultLength idCharGenerator |> Random.map String.fromList


generator prefix =
    stringIdGenerator |> Random.map ((++) prefix)
