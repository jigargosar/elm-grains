module CmdPalette exposing
    ( CmdPalette
    , init
    , queryInputValue
    , rollBy
    , setQueryInput
    , view
    )

import Action exposing (Action)
import BasicsX exposing (callWith, eqBy, eqs, isBlank, justWhen, listToggleMember, maybeWhen, notEq, ter, unless, unwrapMaybe, when)
import Bucket exposing (Bucket)
import Css exposing (px, rem)
import CssCheckBox
import CssElements exposing (..)
import CssIcons
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import Grain exposing (Grain)
import GrainFilter exposing (GrainFilter)
import Html.Styled exposing (Html, div, form, input, styled, text)
import Html.Styled.Attributes exposing (autocomplete, class, classList, css, id, tabindex, value)
import Html.Styled.Events as SE exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E
import Label exposing (Label)
import LabelId exposing (LabelId)
import LabelLayerItem exposing (LabelLayerItem)
import List.Extra as List
import ListIndex exposing (ListIndex)
import Material.Icons.Toggle as MIcons
import Maybe.Extra as Maybe
import Msg exposing (Msg)
import Query exposing (Query)
import QueryPrefix exposing (QueryPrefix)
import SList exposing (SList)


type alias Model =
    { queryInputValue : String, listIdx : ListIndex }


type CmdPalette
    = CmdPalette Model


init actions_ =
    CmdPalette { queryInputValue = "", listIdx = ListIndex.initFromList actions_ }


unwrap : CmdPalette -> Model
unwrap (CmdPalette model) =
    model


queryInputValue =
    unwrap >> .queryInputValue


listIdx =
    unwrap >> .listIdx


modify fn =
    unwrap >> fn >> CmdPalette


setQueryInput val =
    modify (\model -> { model | queryInputValue = val })


filterActionsWithQueryString cp actions =
    let
        boiledQS =
            String.trim (queryInputValue cp) |> String.toLower

        fPred action =
            String.contains boiledQS (action.name |> String.toLower)
    in
    List.filter fPred actions


rollBy offset actions cp =
    let
        filteredActs =
            filterActionsWithQueryString cp actions
    in
    modify (\model -> { model | listIdx = ListIndex.rollBy offset actions model.listIdx })
        cp


attrIf bool attr =
    ter bool attr noAttribute


noAttribute =
    Html.Styled.Attributes.attribute "" ""


maybeAttribute =
    Maybe.withDefault noAttribute


view : List Action -> CmdPalette -> Html Msg
view actions cp =
    let
        viewItem ii a =
            flexRow []
                [ classList [ ( "bg-light-blue", ii.selected ) ] ]
                [ flexRow [] [ class "pa2 flex-grow-1" ] [ text a.name ]
                ]

        filteredActs =
            filterActionsWithQueryString cp actions

        viewList =
            flexCol []
                [ class "pv2" ]
                (ListIndex.selectedMap viewItem filteredActs (listIdx cp))

        onSubmitAttr =
            ListIndex.selected filteredActs (listIdx cp)
                |> unwrapMaybe noAttribute (onSubmit << Msg.CmdPaletteTriggerAction)

        viewForm =
            styled form
                [ Css.minWidth <| px 250 ]
                [ class "flex flex-column", onSubmitAttr ]
                [ viewInput (queryInputValue cp)
                , viewList
                ]
    in
    modelWrapperEl [ tabindex -1 ]
        [ modelBackdropEl [] []
        , modelContentEl [] [ viewForm ]
        ]


viewInput inputValue =
    styled input
        [ Css.border3 (px 1) Css.solid (Css.rgba 0 0 0 0.3) ]
        [ id "cmd-palette-input"
        , class "pa2 flex-grow-1"
        , onInput Msg.CmdPaletteQueryInputChanged
        , value inputValue
        , autocomplete False
        ]
        []
