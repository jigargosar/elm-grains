module StoreGenerator exposing
    ( main
    , storeModule
    )

import Browser
import Html.Styled exposing (div, pre, text)
import Html.Styled.Attributes exposing (class)
import Port
import StructuredWriter exposing (..)


prepend pre list =
    pre ++ list


append post list =
    list ++ post


moduleLine storeName =
    let
        exposedDoc =
            [ string storeName, string storeName ]
                |> StructuredWriter.sepBy ( "( ", ", ", " )" ) True
                |> indent 1

        modulePreDoc =
            [ string "module", string storeName, string "exposing" ]
                |> spaced
    in
    [ modulePreDoc, exposedDoc ] |> breaked


storeModule : String -> Writer
storeModule modelName =
    let
        storeName =
            modelName ++ "Store"
    in
    moduleLine storeName


type alias Model =
    {}


type Msg
    = NoOp


view model =
    div [ class "pa3 " ] [ div [ class "code pre" ] [ text <| write <| storeModule "Grain" ] ]


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = {}
        , update = \_ _ -> {}
        , view = Html.Styled.toUnstyled << view
        }
