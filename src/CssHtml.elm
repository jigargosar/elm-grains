module CssHtml exposing (keyedDiv, noView, viewIf, viewIfLazy)

import Html.Styled exposing (text)
import Html.Styled.Keyed as HK


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


keyedDiv =
    HK.node "div"
