module CssHtml exposing (keyedDiv, noView, viewIf, viewIfLazy, viewMaybe)

import Html.Styled exposing (text)
import Html.Styled.Keyed as HK
import Maybe.Extra as Maybe


noView =
    text ""


viewIf bool content =
    if bool then
        content

    else
        noView


viewMaybe =
    Maybe.unwrap noView


viewIfLazy bool vFn =
    if bool then
        vFn ()

    else
        noView


keyedDiv =
    HK.node "div"
