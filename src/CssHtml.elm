module CssHtml exposing (noView, viewIf, viewIfLazy)

import Html.Styled exposing (text)


noView =
    text ""


viewIf bool content =
    if bool then
        content

    else
        noView


viewIfLazy bool vFn =
    if bool then
        vFn ()

    else
        noView
