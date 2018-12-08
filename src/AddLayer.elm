module AddLayer exposing
    ( AddLayer
    , init
    , setTitle
    , title
    , view
    )

import BasicsX exposing (eqs, isBlank)
import Css exposing (px)
import CssElements exposing (..)
import CssIcons exposing (viewIcon)
import GrainFilter exposing (GrainFilter)
import Html.Styled exposing (Html, div, form, input, styled, text)
import Html.Styled.Attributes exposing (autocomplete, class, classList, disabled, id, tabindex, type_, value)
import Html.Styled.Events as SE exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E
import Material.Icons.Content
import Monocle.Iso as Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import Msg exposing (Msg(..))
import SList exposing (SList)


type alias Model =
    { title : String
    }


type AddLayer
    = AddLayer Model


init : AddLayer
init =
    AddLayer { title = "" }


unwrap (AddLayer model) =
    model


modify fn =
    unwrap >> fn >> AddLayer


setTitle title_ =
    modify <| \model -> { model | title = title_ }


title =
    unwrap >> .title


view (AddLayer model) =
    let
        viewInput =
            styled input
                [ Css.border3 (px 1) Css.solid (Css.rgba 0 0 0 0.3) ]
                [ id "add-layer-input"
                , class "pa2 flex-grow-1 mr2 "
                , onInput AddLayerInputChanged
                , value model.title
                , autocomplete False
                ]
                []

        viewAddBtn =
            iconBtnEl [ type_ "submit", disabled <| isBlank model.title ]
                [ CssIcons.viewIcon CssIcons.add ]

        viewForm =
            styled form
                []
                [ class "flex", onSubmit SubmitAddDialog ]
                [ viewInput
                , viewAddBtn
                ]
    in
    modelWrapperEl [ tabindex -1 ]
        [ modelBackdropEl [] []
        , modelContentEl [] [ viewForm ]
        ]
