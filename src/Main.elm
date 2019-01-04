module Main exposing (main, update)

import ActorId exposing (ActorId)
import Array
import Array.Extra as Array
import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import DatGui
import DecodeX exposing (DecodeResult)
import Direction exposing (Direction)
import Either exposing (Either(..))
import Elm.Parser
import EventX exposing (onKeyDownPD, pNone, pd, sp)
import FireUser exposing (FireUser)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainStore exposing (GrainStore)
import GrainTree exposing (GrainTree)
import GrainTreeView exposing (GrainTreeView)
import GrainView exposing (GrainView)
import GrainZipper__
import HistoryState exposing (HistoryState)
import HotKey as K exposing (HotKey, SoftKey(..))
import Html exposing (Html)
import Json.Decode as D exposing (Decoder)
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
import NotFoundView
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import Route exposing (Route)
import Skeleton
import Tagged
import Task
import Time exposing (Posix)
import TimeX
import Toast exposing (Toast)
import Tree
import Tuple exposing (mapFirst)
import Tuple2
import UrlChange exposing (UrlChange)
import View exposing (ViewModel)



---- MODEL ----


type alias Flags =
    { now : Millis
    , windowSize : WindowSize
    , grains : Value
    , grainCache : Value
    , url : String
    }


initialSeed : Flags -> Seed
initialSeed =
    .now >> Random.initialSeed


type alias Model =
    { toast : Toast
    , route : Route
    , authState : Firebase.AuthState
    , actorId : ActorId
    , grainStore : GrainStore
    , selectedGid : Maybe GrainId
    , lastSelectedGid : Maybe GrainId
    , editGid : Maybe GrainId
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                |> Random.from (initialSeed flags)
                |> Random.always Toast.init
                |> Random.always (Route.fromString flags.url)
                |> Random.always Firebase.initialAuthState
                |> Random.with ActorId.generator
                |> Random.always GrainStore.init
                |> Random.always Nothing
                |> Random.always Nothing
                |> Random.always Nothing
                |> Random.finish
    in
    model
        |> updateGrainStore (GrainStore.Load flags.grainCache)


setRoute route model =
    { model | route = route }


setRouteFromString =
    Route.fromString >> setRoute


mapToast fn model =
    { model | toast = fn model.toast }


setAuthState authState model =
    { model | authState = authState }


grainById : GrainId -> Model -> Maybe Grain
grainById gid =
    .grainStore >> GrainStore.get gid


generateIndependentSeed : Model -> ( Model, Seed )
generateIndependentSeed model =
    let
        ( independentSeed, nextSeed ) =
            Random.step Random.independentSeed model.seed
    in
    ( { model | seed = nextSeed }, independentSeed )


setGrainStore grainStore model =
    { model | grainStore = grainStore }


setEditGid editGid model =
    { model | editGid = editGid }



---- UPDATE ----


type FocusRelativeMsg
    = FR_Forward
    | FR_Backward
    | FR_Parent


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | GrainFocused GrainId Bool
      -- TOAST
    | ToastDismiss
      -- EDIT GRAIN --
    | StartEditing GrainId
    | EndEditing GrainId
      -- ADD GRAIN --
    | NewGrain GrainStore.Add
    | AddNewGrain GrainStore.Add Grain
      -- UPDATE GRAIN --
    | MoveGrain Direction GrainId
    | UpdateGrain GrainStore.Update GrainId
    | UpdateGrainWithNow GrainStore.Update GrainId Posix
      -- GRAIN FOCUS NAVIGATION
    | FocusRelative FocusRelativeMsg GrainTree GrainId
      -- NAVIGATION --
    | RouteTo Route
    | UrlChanged Value
    | BackPressed
      -- AUTH --
    | SignIn
    | SignOut
      -- FIREBASE SUB --
    | Firebase Value
      -- EVENT SUB --
    | KeyDownOnBody Value


routeToGrainTreeMsg gid =
    RouteTo <| Route.GrainTree gid


setParentIdMsg pid gid =
    UpdateGrain (GrainStore.SetParentId pid) gid


setDeletedMsg deleted gid =
    UpdateGrain (GrainStore.SetDeleted deleted) gid


setContentMsg content gid =
    UpdateGrain (GrainStore.SetContent content) gid


autoFocusRouteCmd : Route -> Cmd Msg
autoFocusRouteCmd =
    maybeAutoFocusRouteDomId
        >> unwrapMaybeCmd (BrowserX.focus FocusResult)


maybeAutoFocusRouteDomId : Route -> Maybe String
maybeAutoFocusRouteDomId route =
    case route of
        Route.GrainTree gid ->
            Just <| GrainTreeView.grainDomId gid

        Route.NotFound _ ->
            Nothing


unwrapMaybeCmd cmdFn =
    Maybe.unwrap Cmd.none cmdFn


maybeCmd =
    Maybe.withDefault Cmd.none


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Port.urlChanged UrlChanged
        , Port.fire2Elm Firebase
        , Port.keyDownOnBody <| KeyDownOnBody
        ]


updateError : String -> Model -> ( Model, Cmd Msg )
updateError errString model =
    Return.return (mapToast (Toast.show errString) model)
        (Port.error errString)


updateDecodeError result model =
    result
        |> Result.mapBoth
            (D.errorToString >> updateError >> callWith model)
            (\_ -> Return.singleton model)
        |> Result.merge


handleStringResult :
    Model
    -> Result String (Return Msg Model)
    -> Return Msg Model
handleStringResult model =
    Result.mapError
        (updateError >> callWith model)
        >> Result.merge


handleDecodeResult :
    Model
    -> Result D.Error (Return Msg Model)
    -> Return Msg Model
handleDecodeResult model =
    Result.mapError
        (D.errorToString
            >> updateError
            >> callWith model
        )
        >> Result.merge


focusCmd domId =
    let
        encodeState =
            HistoryState.encoder (HistoryState.init (Just domId))
    in
    Cmd.batch
        [ Browser.Dom.focus domId
            |> Task.mapError (\_ -> "FocusError: domId=" ++ domId)
            |> Task.attempt FocusResult
        , Port.replaceState encodeState
        ]


focusMaybe =
    unwrapMaybeCmd focusCmd



-- GLOBAL KEYBOARD SHORTCUTS


handleKeyDownWhenNothingIsFocused ke model =
    case model.route of
        Route.GrainTree gid ->
            let
                cmd =
                    if K.isArrowKey ke then
                        getSelectedOrLastSelectedGid model
                            |> Maybe.withDefault gid
                            |> focusGidCmd

                    else
                        Cmd.none
            in
            ( model, cmd )

        Route.NotFound _ ->
            Return.singleton model



-- SELECTED GID UPDATE --


setSelectedGid : Maybe GrainId -> Model -> Model
setSelectedGid selectedGid model =
    { model | selectedGid = selectedGid, lastSelectedGid = model.selectedGid }


getSelectedOrLastSelectedGid model =
    model.selectedGid
        |> Maybe.orElseLazy (\_ -> model.lastSelectedGid)


focusGidCmd gid =
    focusCmd <| GrainTreeView.grainDomId gid


focusEditGidCmd gid =
    focusCmd <| GrainTreeView.contentInputDomId gid


focusMaybeGidCmd =
    unwrapMaybeCmd focusGidCmd


focusMaybeGrainCmd =
    unwrapMaybeCmd (Grain.id >> focusGidCmd)



-- FOCUS RELATIVE


focusRelative :
    GrainId
    -> GrainTree
    -> FocusRelativeMsg
    -> Model
    -> Return Msg Model
focusRelative gid tree message model =
    let
        zipper =
            GrainZipper__.fromTree tree

        fn =
            case message of
                FR_Backward ->
                    GrainZipper__.backwardFromRootWhenIdEq

                FR_Forward ->
                    GrainZipper__.forwardFromRootWhenIdEq

                FR_Parent ->
                    GrainZipper__.parentWhenIdEq
    in
    ( model
    , fn gid zipper
        |> focusMaybeGrainCmd
    )



-- GRAIN STORE --


localPersistGrainStoreEffect model =
    Port.setGrainCache <| GrainStore.encoder model.grainStore


effectIf bool fn =
    if bool then
        Return.effect_ fn

    else
        identity


andThenIf bool fn =
    if bool then
        Return.andThen fn

    else
        identity


updateGrainStore :
    GrainStore.Msg
    -> Model
    -> ( Model, Cmd Msg )
updateGrainStore message model =
    let
        setGrainStoreAndPersist : GrainStore -> Model -> Return Msg Model
        setGrainStoreAndPersist newGrainStore =
            Return.singleton
                >> Return.map (setGrainStore newGrainStore)
                >> Return.effect_ localPersistGrainStoreEffect
    in
    GrainStore.update message model.grainStore
        |> Result.mapBoth
            updateError
            setGrainStoreAndPersist
        >> Result.merge
        >> callWith model



-- URL CHANGED


updateUrlPopped :
    String
    -> Maybe HistoryState
    -> Model
    -> Return Msg Model
updateUrlPopped url maybeHS model =
    let
        focusEffect newModel =
            maybeHS
                |> Maybe.andThen HistoryState.focusedDomId
                |> Maybe.orElseLazy
                    (\_ ->
                        maybeAutoFocusRouteDomId newModel.route
                    )
                |> focusMaybe
    in
    Return.singleton (setRouteFromString url model)
        |> Return.effect_ focusEffect


updateUrlChanged : UrlChange -> Model -> Return Msg Model
updateUrlChanged event model =
    if UrlChange.action event == UrlChange.Pop then
        let
            decodeResult =
                D.decodeValue HistoryState.decoder (UrlChange.state event)

            url =
                UrlChange.url event
        in
        updateDecodeError decodeResult model
            |> Return.andThen
                (updateUrlPopped url <| Result.toMaybe decodeResult)

    else
        Return.singleton model



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

        ToastDismiss ->
            Return.singleton (mapToast Toast.dismiss model)

        FocusResult result ->
            result
                |> Result.map (\_ -> Return.singleton model)
                |> handleStringResult model

        FocusRelative msg tree gid ->
            focusRelative gid tree msg model

        GrainFocused gid focused ->
            Return.singleton <|
                if focused then
                    setSelectedGid (Just gid) model

                else
                    model.selectedGid
                        |> Maybe.unwrap model
                            (\oldGid ->
                                if oldGid == gid then
                                    setSelectedGid Nothing model

                                else
                                    model
                            )

        StartEditing gid ->
            ( setEditGid (Just gid) model
            , focusEditGidCmd gid
            )

        EndEditing gid ->
            ( setEditGid Nothing model, focusGidCmd gid )

        NewGrain addMsg ->
            let
                ( newModel, independentSeed ) =
                    generateIndependentSeed model

                grainFromNow : Posix -> Grain
                grainFromNow now =
                    Random.step (Grain.generator now) independentSeed
                        |> Tuple.first
            in
            ( newModel
            , Time.now
                |> Task.map grainFromNow
                |> Task.perform
                    (AddNewGrain addMsg)
            )

        AddNewGrain addMsg grain ->
            model
                |> updateGrainStore (GrainStore.AddGrain addMsg grain)
                |> Return.andThen (update (StartEditing <| Grain.id grain))

        MoveGrain direction gid ->
            update (UpdateGrain (GrainStore.Move direction) gid) model
                |> Return.command (focusGidCmd gid)

        UpdateGrain updateMsg gid ->
            ( model
            , Task.perform
                (UpdateGrainWithNow updateMsg gid)
                Time.now
            )

        UpdateGrainWithNow updateMsg gid now ->
            updateGrainStore
                (GrainStore.UpdateGrain updateMsg gid now)
                model

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_
                    (.route
                        >> Route.toString
                        >> Port.pushUrl
                    )
                |> Return.effect_ (.route >> autoFocusRouteCmd)

        UrlChanged encoded ->
            D.decodeValue UrlChange.decoder encoded
                |> Result.map (updateUrlChanged >> callWith model)
                |> handleDecodeResult model

        Firebase encodedMsg ->
            case Firebase.decodeInbound encodedMsg of
                Firebase.Error errString ->
                    updateError errString model

                Firebase.AuthStateChanged authState ->
                    setAuthState authState model
                        |> Return.singleton

                Firebase.GrainChanges changes ->
                    updateGrainStore
                        (GrainStore.FirebaseChanges changes)
                        model

        SignIn ->
            Return.return model (Firebase.signIn ())

        SignOut ->
            Return.return model (Firebase.signOut ())

        BackPressed ->
            Return.return model (Port.navigateBack ())

        KeyDownOnBody value ->
            D.decodeValue EventX.keyEventDecoder value
                |> Result.map
                    (handleKeyDownWhenNothingIsFocused
                        >> callWith model
                    )
                |> handleDecodeResult model



-- VIEW --


view : Model -> Html Msg
view model =
    View.view (viewModel model)


viewModel : Model -> ViewModel Msg
viewModel model =
    { route = model.route
    , appBarVM = appBarViewModel model
    , createGrainTreeVM =
        \gid ->
            model.grainStore
                |> GrainStore.treeFromGid gid
                |> Maybe.map (grainTreeViewModel model.editGid)
    , toastVM =
        { dismissMsg = ToastDismiss
        , toast = model.toast
        }
    }


appBarViewModel : Model -> View.AppBarView Msg
appBarViewModel model =
    let
        { title, showBackBtn } =
            case model.route of
                Route.GrainTree gid ->
                    if gid == GrainId.root then
                        { title = "Home", showBackBtn = False }

                    else
                        { title = "Focused", showBackBtn = True }

                Route.NotFound _ ->
                    { title = "Oops!", showBackBtn = True }
    in
    { title = title
    , authState = model.authState
    , onBack = maybeBool showBackBtn BackPressed
    , onSignOut = SignOut
    , onSignIn = SignIn
    }


grainTreeViewKeyBindings :
    GrainTree
    -> GrainId
    -> EventX.CustomDecoder Msg
grainTreeViewKeyBindings tree gid =
    let
        treeRootGid =
            Tree.label tree |> Grain.id

        arrowLeftMsg =
            if gid == treeRootGid && gid /= GrainId.root then
                \_ -> BackPressed

            else
                fr FR_Parent

        moveMappings : List ( HotKey, GrainId -> Msg )
        moveMappings =
            List.map
                (Tuple2.double
                    >> Tuple.mapBoth
                        K.metaArrow
                        MoveGrain
                )
                Direction.list

        fr relative =
            FocusRelative relative tree

        bindings : List ( HotKey, GrainId -> Msg )
        bindings =
            [ ( K.arrowDown, fr FR_Forward )
            , ( K.arrowUp, fr FR_Backward )
            , ( K.arrowLeft, arrowLeftMsg )
            , ( K.arrowRight, routeToGrainTreeMsg )
            , ( K.shiftMetaEnter, NewGrain << GrainStore.AddChild )
            , ( K.shiftEnter, NewGrain << GrainStore.AddBefore )
            , ( K.metaEnter, NewGrain << GrainStore.AddAfter )
            , ( K.enter, StartEditing )
            , ( K.esc, EndEditing )
            ]
                ++ moveMappings
    in
    bindings
        |> List.map (Tuple.mapSecond (callWith gid >> pd))
        |> K.bindEachToMsg


grainTreeViewEditKeyBindings :
    GrainTree
    -> GrainId
    -> EventX.CustomDecoder Msg
grainTreeViewEditKeyBindings tree gid =
    let
        bindings : List ( HotKey, GrainId -> Msg )
        bindings =
            [ ( K.enter, EndEditing )
            , ( K.esc, EndEditing )
            ]
    in
    bindings
        |> List.map (Tuple.mapSecond (callWith gid >> pd))
        |> K.bindEachToMsg


grainTreeViewModel : Maybe GrainId -> GrainTree -> GrainTreeView Msg
grainTreeViewModel editGid tree =
    { grainTree = tree
    , routeTo = routeToGrainTreeMsg
    , keyDownCustom = grainTreeViewKeyBindings tree
    , editKeyDownCustom = grainTreeViewEditKeyBindings tree
    , editGid = editGid
    , onContentChanged = \gid content -> setContentMsg content gid
    }


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
