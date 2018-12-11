module CssHtml exposing (..)


import Html.Styled exposing (text)
noView = text ""

viewIf bool content =
    if bool then
        content
    else
        noView
