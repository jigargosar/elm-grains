module CmdPaletteOld exposing
    ( Action(..)
    , CmdPalette
    , EditGrainAction(..)
    , init
    , query
    , rollBy
    , selected
    , setQuery
    , view
    )

import BasicsX exposing (callWith, eqBy, eqs, isBlank, justWhen, listToggleMember, maybeWhen, notEq, ter, unless, unwrapMaybe, when)
import Bucket exposing (Bucket)
import Css exposing (px, rem)
import CssCheckBox
import CssElements exposing (..)
import CssIcons
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import Grain exposing (Grain)
import GrainFilter exposing (GrainFilter)
import HasFilteredGrains exposing (HasFilteredGrains)
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
import Material.Icons.Toggle as MIcons
import Maybe.Extra as Maybe
import Msg exposing (Msg)
import Query exposing (Query)
import QueryPrefix exposing (QueryPrefix)
import SList exposing (SList)


type EditGrainAction
    = ToggleLabel Label
    | MoveToBucket Bucket


type Action
    = SetGrainFilter GrainFilter
    | EditGrain EditGrainAction


type alias ActionList =
    SList Action


type alias Model =
    { query : Query
    , list : SList Action
    , allLabels : List Label
    }


type CmdPalette
    = CmdPalette Model


init :
    { labels : List Label
    , filters : List GrainFilter
    , allLabels : List Label
    , queryPrefix : QueryPrefix
    }
    -> CmdPalette
init ({ labels, filters, allLabels } as c) =
    CmdPalette
        { query = Query.init c.queryPrefix
        , list =
            SList.fromList
                (List.map EditGrain
                    (List.map ToggleLabel labels ++ List.map MoveToBucket Bucket.all)
                    ++ List.map SetGrainFilter filters
                )
        , allLabels = allLabels
        }


unwrap : CmdPalette -> Model
unwrap (CmdPalette model) =
    model


list =
    unwrap >> .list


selected =
    list >> SList.selected


query =
    unwrap >> .query


queryInputValue =
    query >> Query.inputFieldValue


queryPrefix =
    query >> Query.prefix


actionDisplayName action =
    case action of
        EditGrain edit ->
            case edit of
                ToggleLabel label ->
                    "Toggle Label " ++ Label.displayName label

                MoveToBucket bucket ->
                    "Move To " ++ Bucket.displayName bucket

        SetGrainFilter filter ->
            "Filter " ++ (GrainFilter.viewModel filter |> .name)


actionToQP action =
    case action of
        EditGrain edit ->
            case edit of
                ToggleLabel _ ->
                    QueryPrefix.Label

                MoveToBucket _ ->
                    QueryPrefix.Bucket

        SetGrainFilter _ ->
            QueryPrefix.GrainFilter


actionMatchesPrefix qp action =
    qp == QueryPrefix.None || qp == actionToQP action


actionEnabledForGrain cmdPalette hasFG action =
    case action of
        EditGrain edit ->
            let
                maybeGrain =
                    HasFilteredGrains.selectedGrain hasFG

                maybeToBool =
                    unwrapMaybe False (always True)
            in
            case edit of
                ToggleLabel _ ->
                    maybeToBool maybeGrain

                MoveToBucket bucket ->
                    maybeGrain |> unwrapMaybe True (Grain.bucket >> notEq bucket)

        SetGrainFilter _ ->
            True


actionPred hasFG palette action =
    let
        matchesPrefixOperator =
            actionMatchesPrefix (queryPrefix palette) action

        matchesQueryString =
            Query.matchesString (actionDisplayName action) (query palette)

        isEnabled =
            actionEnabledForGrain palette hasFG action
    in
    matchesPrefixOperator && matchesQueryString && isEnabled


modify : (Model -> Model) -> CmdPalette -> CmdPalette
modify fn =
    unwrap >> fn >> CmdPalette


modifyQuery fn =
    modify (\model -> { model | query = fn model.query })


setQuery : String -> HasFilteredGrains x -> CmdPalette -> CmdPalette
setQuery iv hasFG oldCmdPalette =
    oldCmdPalette
        |> modifyQuery (Query.modifyWithPrefixedInputValue iv)
        |> unless (eqBy query oldCmdPalette) (selectHead hasFG)


selectHead : HasFilteredGrains x -> CmdPalette -> CmdPalette
selectHead hasFG layer =
    modifyList (SList.selectFilteredHead (actionPred hasFG layer)) layer


modifyList : (ActionList -> ActionList) -> CmdPalette -> CmdPalette
modifyList fn =
    modify (\model -> { model | list = fn model.list })


rollBy offset hasFG layer =
    modifyList (SList.filterAndRollBy (actionPred hasFG layer) offset) layer


view : HasFilteredGrains x -> CmdPalette -> Html Msg
view hasFG layer =
    let
        viewItemWithIsSelected isSelected =
            let
                maybeGrain =
                    HasFilteredGrains.selectedGrain hasFG
            in
            viewItem maybeGrain isSelected

        viewList =
            flexCol []
                [ class "pv2" ]
                (list layer
                    |> SList.filterMapCSToList
                        (actionPred hasFG layer)
                        (viewItemWithIsSelected True)
                        (viewItemWithIsSelected False)
                )

        viewForm =
            styled form
                [ Css.minWidth <| px 250 ]
                [ class "flex flex-column", onSubmit Msg.CmdPaletteSubmit ]
                [ viewInput (queryInputValue layer)
                , viewList
                ]
    in
    modelWrapperEl [ tabindex -1 ]
        [ modelBackdropEl [] []
        , modelContentEl [] [ viewForm ]
        ]


viewItem maybeGrain isSelected action =
    let
        render content =
            itemEl
                [ classList [ ( "bg-light-blue", isSelected ) ]
                ]
                [ content ]
    in
    case action of
        EditGrain edit ->
            let
                egContent grain =
                    case edit of
                        ToggleLabel label ->
                            viewToggleLabelAI
                                (Grain.labelIds grain)
                                label

                        MoveToBucket bucket ->
                            viewMoveToBucketAI bucket
            in
            unwrapMaybe (text "") (render << egContent) maybeGrain

        SetGrainFilter filter ->
            render <| viewSetGrainFilterAI filter


viewSwitch checked =
    let
        highColor =
            ter checked "dodgerblue" "gray"

        jc =
            ter checked Css.flexEnd Css.flexStart

        txt =
            ter checked "ON" "OFF"
    in
    flexRowIC
        [ Css.property "background-color" highColor
        , Css.width <| rem 2
        , Css.justifyContent jc
        ]
        [ class "br-pill " ]
        [ flexRowIC
            [ Css.height <| rem 1.2

            --            , Css.padding <| px 4
            , Css.width <| rem 1.2
            , Css.property "color" highColor
            , Css.property "border-color" highColor
            , Css.letterSpacing <| px -1
            ]
            [ class "code f7 bg-white ba br-pill" ]
            [ text "" ]
        ]


viewTitle title =
    div [ class "pv2" ] [ text title ]


itemElClasses =
    "pointer ph2 "


selectedItemElClasses =
    itemElClasses ++ " bg-light-blue"


itemEl attrs =
    div (class itemElClasses :: attrs)


selectedItemEl attrs =
    div (class selectedItemElClasses :: attrs)


viewToggleLabelAI checkedLabelIds label =
    let
        titleText =
            "Toggle Label " ++ Label.displayName label

        isChecked =
            List.any (Label.hasId >> callWith label) checkedLabelIds
    in
    flexRowIC [ Css.justifyContent Css.spaceBetween ]
        []
        [ viewTitle titleText
        , viewSwitch isChecked
        ]


viewMoveToBucketAI bucket =
    let
        titleText =
            "Move To " ++ Bucket.displayName bucket
    in
    flexRowIC [] [] [ viewTitle titleText ]


viewSetGrainFilterAI filter =
    let
        titleText =
            "Filter " ++ (GrainFilter.viewModel filter |> .name)
    in
    flexRowIC [] [] [ viewTitle titleText ]


styledWithClass styles cls attrs =
    styled div styles (class cls :: attrs)


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
