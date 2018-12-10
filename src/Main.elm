module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (num, pct, px)
import CssElements exposing (..)
import CssElevation exposing (elevation)
import CssIcons exposing (viewIcon)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import EventX exposing (onKeyDownPD)
import Grain exposing (Grain)
import GrainListView exposing (GrainListView)
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
import Msg exposing (Msg(..))
import Port
import Random exposing (Generator, Seed)
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import Tagged
import Task
import Tuple exposing (mapFirst)
import UpdateHandler exposing (..)



---- MODEL ----


type alias Flags =
    { now : Millis
    , windowSize : WindowSize
    , grains : Value
    }


initialSeed : Flags -> Seed
initialSeed =
    .now >> Random.initialSeed


type alias Model =
    { hasFocusIn : Bool
    , grainStore : GrainStore
    }


initWithGrainStore : GrainStore -> Model
initWithGrainStore initialGrainStore =
    { hasFocusIn = False
    , grainStore = initialGrainStore
    }


generator : Generator Model
generator =
    Random.map initWithGrainStore GrainStore.generator


init : Flags -> Return Msg Model
init flags =
    Random.step generator (initialSeed flags)
        |> Tuple.mapSecond (always Cmd.none)



---- UPDATE ----


focusBaseLayerCmd =
    Browser.Dom.focus "base-layer"
        |> Task.attempt (\_ -> NoOp)


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.succeed BrowserAnyKeyDown ]


handle :
    Msg
    -> HandlerConfig Msg Model
    -> HandlerConfig Msg Model
handle message =
    case message of
        NoOp ->
            identity

        LogError errMsg ->
            andDo (Port.error errMsg)

        BrowserAnyKeyDown ->
            andDoWhen (.hasFocusIn >> not) focusBaseLayerCmd

        BaseLayerFocusInChanged hasFocusIn ->
            mapModel (\model -> { model | hasFocusIn = hasFocusIn })

        AddNewClicked ->
            let
                subConfig =
                    { handler = GrainStore.handle
                    , toMsg = GrainStoreSub
                    , get = .grainStore
                    , set = \newGrainStore model -> { model | grainStore = newGrainStore }
                    }
            in
            dispatchSub GrainStore.newMsg subConfig

        GrainStoreSub msg ->
            identity

        GrainStoreSubReply ->
            identity


keyBinding model =
    K.bindEachToMsg <|
        [ ( K.arrowUp, ( NoOp, True ) )
        , ( K.arrowDown, ( NoOp, True ) )
        ]


view : Model -> Html Msg
view model =
    flexCol
        [ Css.flexShrink <| num 0
        , Css.minWidth <| pct 100
        , Css.minHeight <| pct 100
        ]
        [ id "base-layer"
        , class "sans-serif"
        , SA.fromUnstyled <| EventX.onFocusIn <| BaseLayerFocusInChanged True
        , SA.fromUnstyled <| EventX.onFocusOut <| BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <|
            EventX.onKeyDownPD <|
                keyBinding model
        ]
        [ button [ onClick AddNewClicked ] [ text "add new empty" ]
        , GrainListView.view (mapStateToGrainListView model)
        ]


mapStateToGrainListView : Model -> GrainListView
mapStateToGrainListView model =
    { grainList = model.grainStore |> GrainStore.allAsList }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = toElmUpdateFn handle

        --        , update = updateDispatcher
        , subscriptions = subscriptions
        }
