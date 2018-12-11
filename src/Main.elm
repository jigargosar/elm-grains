module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px)
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
import Random.Pipeline as Random
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)
import Tagged
import Task
import Toast exposing (Toast)
import Tuple exposing (mapFirst)



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
    { grainStore : GrainStore
    , hasFocusIn : Bool
    , toast : Toast
    , seed : Seed
    }


initialHasFocusIn =
    False


init : Flags -> Return Msg Model
init flags =
    Model
        |> Random.from (initialSeed flags)
        |> Random.with GrainStore.generator
        |> Random.always initialHasFocusIn
        |> Random.always (Toast.visible "App Init")
        |> Random.finish
        |> Return.singleton


setGrainStore grainStore model =
    { model | grainStore = grainStore }


mapToast fn model =
    { model | toast = fn model.toast }


setErrorToast title =
    mapToast (Toast.showWithTitle title)



---- UPDATE ----


focusBaseLayerCmd =
    Browser.Dom.focus "base-layer"
        |> Task.attempt (\_ -> NoOp)


focusGrain =
    GrainListView.focusGrain >> Task.attempt (\_ -> NoOp)


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.succeed BrowserAnyKeyDown ]


grainStoreSubConfig =
    { subUpdate = GrainStore.update
    , get = .grainStore
    , set = setGrainStore
    , toMsg = GrainStoreSubMsg
    , replyToMsg = GrainStoreReply
    , update = update
    }


logErrorString err =
    R3.do (Port.error err)
        >> R3.map (setErrorToast err)


update : Msg -> Return3F Msg Model ()
update message =
    case message of
        NoOp ->
            identity

        FocusResult (Ok ()) ->
            identity

        FocusResult (Err err) ->
            logErrorString "Dom Focus Error"

        BrowserAnyKeyDown ->
            R3.doWhen (.hasFocusIn >> not) focusBaseLayerCmd

        BaseLayerFocusInChanged hasFocusIn ->
            R3.map (\model -> { model | hasFocusIn = hasFocusIn })

        AddNewClicked ->
            R3.dispatch GrainStore.createNewGrain grainStoreSubConfig

        GrainStoreSubMsg msg ->
            R3.sub msg grainStoreSubConfig

        GrainStoreReply reply ->
            case reply of
                GrainStore.NoReply ->
                    identity

                GrainStore.NewGrainAddedReply grain ->
                    R3.do (focusGrain grain)

        ToastDismiss ->
            R3.map (mapToast Toast.dismiss)


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
        , viewToast model.toast
        ]


viewToast toast =
    let
        toastView =
            { dismiss = ToastDismiss }
    in
    Toast.view toastView toast


mapStateToGrainListView : Model -> GrainListView
mapStateToGrainListView model =
    { grainList = model.grainStore |> GrainStore.allAsList }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = R3.toElmUpdate update

        --        , update = updateDispatcher
        , subscriptions = subscriptions
        }
