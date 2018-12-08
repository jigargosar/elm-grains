module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (Style, alignItems, alignSelf, backgroundColor, center, color, column, displayFlex, flexDirection, flexGrow, fontSize, hex, int, justifyContent, maxWidth, minHeight, minWidth, num, overflow, padding, pct, px, rem, rgb, rgba, vh, vw, zero)
import CssElements exposing (..)
import CssIcons exposing (viewIcon)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import Cursor exposing (Cursor)
import DecodeX exposing (DecodeResult)
import DomX
import Either exposing (Either(..))
import Elevation exposing (elevation)
import EventX exposing (onKeyDownPD)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainStore exposing (GrainStore)
import HotKey as K exposing (SoftKey(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Html.Styled.Keyed
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E exposing (Value)
import List.Extra as List
import ListIndex exposing (ListIndex)
import Material.Icons.Action as MIcons
import Material.Icons.Alert as MIcons
import Material.Icons.Content as MIcons
import Material.Icons.Editor as MIcons
import Material.Icons.Navigation as MIcons
import Material.Icons.Toggle as MIcons
import Maybe.Extra as Maybe
import Msg exposing (GM(..), Msg(..))
import Palette exposing (black, black10, white)
import Port
import QueryPrefix
import Random exposing (Generator, Seed)
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import SList exposing (SList)
import Task
import Theme exposing (spacingUnit)
import TimeX exposing (Millis)
import Tuple exposing (mapFirst)
import UpdateHandler exposing (..)



---- MODEL ----


type alias Flags =
    { now : Millis
    , windowSize : WindowSize
    , grains : Value
    }


type alias Model =
    { grains : GrainStore
    , hasFocusIn : Bool
    , inputValue : String
    , gLIdx : ListIndex
    }


init : Flags -> Return Msg Model
init flags =
    Return.map
        (\grains ->
            { grains = grains
            , hasFocusIn = False
            , inputValue = ""
            , gLIdx = ListIndex.empty
            }
        )
        (GrainStore.decode flags.grains)
        |> Return.effect_ focusSelectedGrain


selectedGrain model =
    ListIndex.selected (currentGrainList model) model.gLIdx


currentGrainList =
    .grains >> GrainStore.items


setGrains grains model =
    { model | grains = grains }


overGrains : (GrainStore -> GrainStore) -> Model -> Model
overGrains fn model =
    setGrains (fn model.grains) model


addGrain : Grain -> Model -> Model
addGrain grain =
    overGrains (GrainStore.prepend grain)


overGrainWithId : GrainId -> (Grain -> Grain) -> Model -> Model
overGrainWithId gid fn =
    overGrains (GrainStore.update gid fn)



---- UPDATE ----


mapGrains fn =
    mapModel <| overGrains fn


addGrainWithInsertPosition ( insertPos, grain ) =
    mapModel (overGrains (GrainStore.insertAt ( insertPos, grain )))
        >> andThenDo cacheGrains
        >> andDo (focusGrain grain)


focusGrain =
    GrainStore.grainDomId >> Browser.Dom.focus >> Task.attempt (\_ -> NoOp)


focusMaybeGrain =
    unwrapMaybe Cmd.none focusGrain


focusSelectedGrain =
    selectedGrain >> focusMaybeGrain


andFocusSelectedGrain =
    andThenDo focusSelectedGrain


updateGrain gid fn =
    mapModel (overGrainWithId gid fn)
        >> andThenDo cacheGrains


cacheGrains : Model -> Cmd msg
cacheGrains =
    .grains >> GrainStore.cacheCmd


focusBaseLayerCmd =
    Browser.Dom.focus "base-layer"
        |> Task.attempt (\_ -> NoOp)


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.succeed BrowserAnyKeyDown ]


updateF message =
    case message of
        NoOp ->
            identity

        LogError errMsg ->
            andDo (Port.error errMsg)

        BrowserAnyKeyDown ->
            andDoWhen (.hasFocusIn >> not) focusBaseLayerCmd

        BaseLayerFocusInChanged hasFocusIn ->
            mapModel (\model -> { model | hasFocusIn = hasFocusIn })

        InputChanged inputValue ->
            mapModel (\model -> { model | inputValue = inputValue })

        InputSubmit ->
            dispatch (SubGM GMNew)

        SubGM msg ->
            case msg of
                GMNew ->
                    andDoWith .inputValue
                        (\title ->
                            Grain.newGeneratorWithTitleAndInsertPosition title Grain.Head
                                |> Task.perform (SubGM << GMOnGen)
                        )

                GMOnGen gen ->
                    andDo (Random.generate (SubGM << GMAdd) gen)

                GMAdd ( insertPosition, grain ) ->
                    let
                        _ =
                            Debug.log "insertPosition" insertPosition
                    in
                    addGrainWithInsertPosition ( insertPosition, grain )

                GMTitle gid newTitle ->
                    updateGrain gid (Grain.setTitle newTitle)

                GMNewAfter gid ->
                    andDo
                        (Grain.newGeneratorWithTitleAndInsertPosition "" (Grain.After gid)
                            |> Task.perform (SubGM << GMOnGen)
                        )

                GMDeleteIfEmpty gid ->
                    mapGrains (GrainStore.deleteIfEmpty gid)
                        >> andFocusSelectedGrain

                GMGrainFocused gid ->
                    mapModel
                        (\model ->
                            let
                                newGLIdx =
                                    ListIndex.selectBy (Grain.hasId gid)
                                        (currentGrainList model)
                                        model.gLIdx
                            in
                            { model | gLIdx = newGLIdx }
                        )

        Prev ->
            identity

        Next ->
            identity


keyBinding model =
    K.bindEachToMsg <|
        [ ( K.arrowUp, ( NoOp, True ) )
        , ( K.arrowDown, ( NoOp, True ) )
        ]


view : Model -> Html Msg
view model =
    styled div
        [ Css.flexShrink <| num 0
        , Css.minWidth <| pct 100
        , Css.minHeight <| pct 100
        ]
        [ id "base-layer"
        , class "sans-serif flex flex-column"
        , SA.fromUnstyled <| EventX.onFocusIn <| BaseLayerFocusInChanged True
        , SA.fromUnstyled <| EventX.onFocusOut <| BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <|
            EventX.onKeyDownPD <|
                keyBinding model
        ]
        [ viewBase model
        ]


viewBase : Model -> Html Msg
viewBase model =
    styled div
        []
        [ class "flex flex-column items-center" ]
        [ styled div
            [ Css.width <| px 400 ]
            [ class "flex flex-column pv3" ]
            [ viewGrainList (currentGrainList model)
            ]
        ]


viewGrainList : List Grain -> Html Msg
viewGrainList list =
    let
        viewItemChildren =
            if List.isEmpty list then
                [ ( "newInputField"
                  , Html.form [ class "flex flex-column", onSubmit InputSubmit ]
                        [ input [ onInput InputChanged ] []
                        ]
                  )
                ]

            else
                List.map (\g -> ( GrainStore.grainDomId g, viewGrainItem False g )) list
    in
    Html.Styled.Keyed.node "div"
        [ id "grains-container", class "flex flex-column pv2" ]
        viewItemChildren


viewGrainItem selected grain =
    let
        title =
            Grain.title grain

        gid =
            Grain.id grain

        viewGrainTitle =
            flexRow []
                [ class "f4 pa1" ]
                [ text <|
                    if isBlank title then
                        "<no title>"

                    else
                        title
                ]
    in
    styled input
        [ Css.borderBottom3 (px 1) Css.solid (Css.rgba 0 0 0 0.3) ]
        [ id (Grain.idAsString grain)
        , class "pa2 bn "
        , classList [ ( "bg-lightest-blue", selected ) ]
        , onInput (SubGM << GMTitle gid)
        , value title
        , autocomplete False
        , onFocus <| SubGM <| GMGrainFocused gid
        , SA.fromUnstyled <|
            onKeyDownPD <|
                K.bindEachToMsg
                    [ ( K.enter, ( SubGM <| GMNewAfter gid, True ) )
                    , ( ( [], "Backspace" ), ( SubGM <| GMDeleteIfEmpty gid, False ) )
                    ]
        ]
        []



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = toElmUpdateFn updateF

        --        , update = updateDispatcher
        , subscriptions = subscriptions
        }
