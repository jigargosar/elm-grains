module StyledEvents exposing (onFocusIn, onFocusOut, onKeyDownPD)

import EventX
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)


onFocusIn =
    SA.fromUnstyled << EventX.onFocusIn


onFocusOut =
    SA.fromUnstyled << EventX.onFocusOut


onKeyDownPD =
    SA.fromUnstyled << EventX.onKeyDownPD
