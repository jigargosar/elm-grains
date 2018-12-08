module Icons exposing (checkCircleOutline, circleOutline, squareOutline, trash)

import Element exposing (html)
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (color, d, fill, viewBox)


svg : List (Html.Attribute msg) -> List (Svg msg) -> Html msg
svg attrs =
    Svg.svg <| [ fill "currentColor" ] ++ attrs


trash =
    svg
        [ attribute "height" "24"
        , viewBox "0 0 24 24"
        , attribute "width" "24"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ Svg.path [ d "M6 19c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7H6v12zM19 4h-3.5l-1-1h-5l-1 1H5v2h14V4z" ]
            []
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]
        |> html


squareOutline =
    svg
        [ attribute "height" "24"
        , viewBox "0 0 24 24"
        , attribute "width" "24"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ path [ d "M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z" ]
            []
        , path [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]
        |> html


circleOutline =
    svg
        [ attribute "height" "24"
        , viewBox "0 0 24 24"
        , attribute "width" "24"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z" ]
            []
        , path [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]
        |> html


checkCircleOutline =
    svg
        [ attribute "height" "24"
        , viewBox "0 0 24 24"
        , attribute "width" "24"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ path [ d "M0 0h24v24H0V0zm0 0h24v24H0V0z", fill "none" ]
            []
        , path [ d "M16.59 7.58L10 14.17l-3.59-3.58L5 12l5 5 8-8zM12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z" ]
            []
        ]
        |> html
