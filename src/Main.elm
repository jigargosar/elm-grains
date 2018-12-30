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
import GrainBuilder exposing (GrainBuilder)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainMorePopupView exposing (GrainMorePopupView)
import GrainStore exposing (GrainStore)
import GrainTreeView exposing (GrainTreeView)
import GrainView exposing (GrainView)
import GrainZipper exposing (GrainTree)
import HistoryState exposing (HistoryState)
import HotKey as K exposing (HotKey, SoftKey(..))
import Html exposing (Html)
import InlineEditGrain exposing (InlineEditGrain)
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
import MoveGrainPopupView exposing (MoveGrainPopupView)
import NotFoundView
import Popup exposing (Popup)
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import Route exposing (Route)
import SavedGrain exposing (SavedGrain)
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
    , popup : Popup
    , inlineEditGrain : InlineEditGrain
    , grainStore : GrainStore
    , selectedGid : Maybe GrainId
    , lastSelectedGid : Maybe GrainId
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
                |> Random.always Popup.NoPopup
                |> Random.always InlineEditGrain.initialValue
                |> Random.always GrainStore.init
                |> Random.always Nothing
                |> Random.always Nothing
                |> Random.finish
    in
    model
        |> update (UpdateGrainStore <| GS_Load flags.grainCache)


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


setNewSeed newSeed model =
    { model | seed = newSeed }


dismissPopup model =
    { model | popup = Popup.NoPopup }


setGrainStore grainStore model =
    { model | grainStore = grainStore }



---- UPDATE ----


type GrainStoreMsg
    = GS_UpdateGrain GrainStore.Update GrainId Posix
    | GS_AddGrain GrainStore.Add Grain
    | GS_FirebaseChanges (List GrainChange)
    | GS_Load Value


type InlineEditGrainMsg
    = IE_Start
    | IE_Content String
    | IE_Submit
    | IE_Discard
    | IE_KeyboardFocus Bool


type PopupMsg
    = PM_SetGrainParent GrainId Grain.ParentId
    | PM_MoveGrain GrainId Direction
    | PM_SetGrainDeleted GrainId Bool
    | PM_RouteToGrain GrainId
    | PM_Dismiss
    | PM_Open Popup


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
      -- ADD GRAIN --
    | NewGrain GrainStore.Add
    | NewGrainStep (GrainBuilder GrainStore.Add)
      -- UPDATE GRAIN --
    | UpdateGrain GrainStore.Update GrainId
    | GrainSet Grain.Set GrainId
    | UpdateGrainStore GrainStoreMsg
    | UpdateInlineEditGrain GrainId InlineEditGrainMsg
    | DragGrain GrainId
      -- GRAIN FOCUS NAVIGATION
    | FocusRelative FocusRelativeMsg GrainTree GrainId
      -- POPUP
    | Popup PopupMsg
    | DismissPopupAnd Msg
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


routeToMsg route =
    RouteTo route


routeToGrainTreeMsg gid =
    routeToMsg <| Route.GrainTree gid


autoFocusRouteCmd : Route -> Cmd Msg
autoFocusRouteCmd =
    maybeAutoFocusRouteDomId >> unwrapMaybeCmd (BrowserX.focus FocusResult)


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


performWithNow nowToMsg =
    Task.perform nowToMsg Time.now


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
            GrainZipper.fromTree tree

        fn =
            case message of
                FR_Backward ->
                    GrainZipper.backwardFromRootWhenIdEq

                FR_Forward ->
                    GrainZipper.forwardFromRootWhenIdEq

                FR_Parent ->
                    GrainZipper.parentWhenIdEq
    in
    ( model
    , fn gid zipper
        |> focusMaybeGrainCmd
    )



-- POPUP UPDATE
-- UPDATE InlineEditGrain --


setInlineEditGrain : InlineEditGrain -> Model -> Model
setInlineEditGrain inlineEditGrain model =
    { model | inlineEditGrain = inlineEditGrain }


initInlineEdit grain =
    setInlineEditGrain <| InlineEditGrain.startEditing grain


updateInlineEditGrain gid msg model =
    let
        handleResult =
            Result.map
                (setInlineEditGrain
                    >> callWith model
                    >> Return.singleton
                )
                >> handleStringResult model
    in
    case msg of
        IE_Start ->
            let
                inlineEdit grain =
                    let
                        ieDomId =
                            GrainTreeView.contentInputDomId gid
                    in
                    ( initInlineEdit grain model
                    , Cmd.batch
                        [ focusCmd ieDomId
                        , Port.autoSize ieDomId
                        ]
                    )
            in
            grainById gid model
                |> Result.fromMaybe "IE_Start: Grain Not Found"
                |> Result.map inlineEdit
                |> handleStringResult model

        IE_Submit ->
            let
                mapResult ( gid_, content, inlineEditGrain ) =
                    let
                        focusEditedGidCmd =
                            focusGidCmd gid_

                        setContentCmd =
                            performGrainSetContent content gid_
                    in
                    ( setInlineEditGrain inlineEditGrain model
                    , Cmd.batch [ setContentCmd, focusEditedGidCmd ]
                    )
            in
            InlineEditGrain.endEditing model.inlineEditGrain
                |> Result.map mapResult
                |> handleStringResult model

        IE_Discard ->
            InlineEditGrain.discard model.inlineEditGrain
                |> handleResult
                |> Return.command (focusGidCmd gid)

        IE_Content content ->
            InlineEditGrain.onContentChange content model.inlineEditGrain
                |> handleResult

        IE_KeyboardFocus focused ->
            InlineEditGrain.setFocused focused model.inlineEditGrain
                |> handleResult



-- GRAIN STORE --


performGrainMove direction gid =
    performGrainUpdate (GrainStore.Move direction) gid


performGrainSetContent content gid =
    performGrainSet (Grain.SetContent content) gid


performGrainSet setMsg gid =
    performGrainUpdate (GrainStore.Set setMsg) gid


performGrainUpdate msg gid =
    Task.perform
        (UpdateGrainStore
            << GS_UpdateGrain msg gid
        )
        Time.now


localPersistGrainStoreEffect model =
    Port.setGrainCache <| GrainStore.encoder model.grainStore


firePersistUnsavedGrainsEffect model =
    let
        dirtyGrains =
            model.grainStore
                |> GrainStore.toRawList
                |> List.filter SavedGrain.needsPersistence
    in
    if List.isEmpty dirtyGrains then
        Cmd.none

    else
        dirtyGrains
            |> E.list SavedGrain.encoder
            |> Port.persistSavedGrainList


addBuiltGrain ( addMsg, grain ) model =
    updateGrainStore
        (GS_AddGrain addMsg grain)
        model


updateGrainStore :
    GrainStoreMsg
    -> Model
    -> ( Model, Cmd Msg )
updateGrainStore message model =
    let
        setGrainStoreAndPersist newGrainStore =
            Return.singleton
                >> Return.map (setGrainStore newGrainStore)
                >> Return.effect_ localPersistGrainStoreEffect
                >> Return.effect_ firePersistUnsavedGrainsEffect

        handleResult =
            Result.map (setGrainStoreAndPersist >> callWith model)
                >> handleStringResult model
    in
    case message of
        GS_AddGrain msg grain ->
            GrainStore.addNew msg
                grain
                model.grainStore
                |> handleResult

        GS_UpdateGrain msg gid now ->
            GrainStore.update msg
                gid
                now
                model.grainStore
                |> handleResult

        GS_FirebaseChanges changeList ->
            GrainStore.updateFromFirebaseChangeList changeList
                model.grainStore
                |> handleResult

        GS_Load encoded ->
            GrainStore.load encoded |> handleResult



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

        NewGrain addMsg ->
            ( model, GrainBuilder.init addMsg NewGrainStep )

        NewGrainStep builder ->
            builder
                |> GrainBuilder.continue NewGrainStep
                |> Either.unpack
                    (Return.return model)
                    (addBuiltGrain >> callWith model)

        --|> Return.andThen (updateInlineEditGrain gid IE_Start)
        GrainSet msg gid ->
            update (UpdateGrain (GrainStore.Set msg) gid)
                model

        UpdateGrain msg gid ->
            ( model, performGrainUpdate msg gid )

        UpdateGrainStore msg ->
            updateGrainStore msg model

        UpdateInlineEditGrain gid msg ->
            updateInlineEditGrain gid msg model

        DragGrain gid ->
            Return.singleton model

        DismissPopupAnd msg ->
            dismissPopup model
                |> update msg

        Popup msg ->
            case msg of
                PM_SetGrainParent gid parentId ->
                    dismissPopup model
                        |> update
                            (GrainSet (Grain.SetParentId parentId) gid)

                PM_MoveGrain gid direction ->
                    dismissPopup model
                        |> update
                            ((UpdateGrain <|
                                GrainStore.Move direction
                             )
                                gid
                            )

                PM_SetGrainDeleted gid deleted ->
                    dismissPopup model
                        |> (update <|
                                GrainSet
                                    (Grain.SetDeleted deleted)
                                    gid
                           )

                PM_RouteToGrain gid ->
                    dismissPopup model
                        |> update (routeToGrainTreeMsg gid)

                PM_Dismiss ->
                    dismissPopup model
                        |> Return.singleton

                PM_Open popup ->
                    { model | popup = popup }
                        |> Return.singleton

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
                    updateGrainStore (GS_FirebaseChanges changes) model

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
    , popup = model.popup
    , appBarVM = appBarViewModel model
    , createGrainTreeVM =
        \gid ->
            model.grainStore
                |> GrainStore.treeFromGid gid
                |> Maybe.map grainTreeViewModel
    , createGrainMorePopupVM =
        \gid ->
            grainById gid model
                |> Maybe.map (grainMorePopupViewModel model)
    , createGrainMovePopupVM =
        \gid ->
            grainById gid model
                |> Maybe.map (moveGrainPopupViewModel model)
    , toastVM =
        { dismissMsg = ToastDismiss
        , toast = model.toast
        }
    }


sort array =
    let
        length =
            Array.length array
    in
    if length < 2 then
        array

    else
        Array.slice 0 (length // 2) array


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


grainMorePopupViewModel : Model -> Grain -> GrainMorePopupView Msg
grainMorePopupViewModel model grain =
    let
        deleted =
            Grain.deleted grain

        gid =
            Grain.id grain
    in
    { editMsg = Popup <| PM_RouteToGrain gid
    , moveUpMsg = Popup <| PM_MoveGrain gid Direction.Up
    , moveDownMsg = Popup <| PM_MoveGrain gid Direction.Down
    , moveToMsg = Popup <| PM_Open (Popup.GrainMovePopup gid)
    , toggleDeleteMsg =
        Popup <|
            PM_SetGrainDeleted gid (not deleted)
    , dismissMsg = Popup PM_Dismiss
    , deleted = deleted
    }


moveGrainPopupViewModel : Model -> Grain -> MoveGrainPopupView Msg
moveGrainPopupViewModel model grain =
    let
        gid =
            Grain.id grain

        otherGrains =
            GrainStore.rejectSubTreeAndFlatten grain model.grainStore
                |> Debug.log "otherGrains"
    in
    { grain = grain
    , otherGrains = otherGrains
    , isSelected = Grain.isParentOf grain
    , dismissMsg = DismissPopupAnd NoOp
    , setParentMsg =
        \pid ->
            DismissPopupAnd <| GrainSet (Grain.SetParentId pid) gid
    , setParentToRootMsg =
        DismissPopupAnd <|
            GrainSet (Grain.SetParentId Grain.rootIdAsParentId) gid
    }



--grainTreeViewModel : GrainTree -> GrainTreeView Msg


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
                        (UpdateGrain << GrainStore.Move)
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
            ]
                ++ moveMappings
    in
    bindings
        |> List.map (Tuple.mapSecond (callWith gid >> pd))
        |> K.bindEachToMsg


grainTreeViewModel tree =
    { grainTree = tree
    , routeTo = routeToGrainTreeMsg
    , keyDownCustom = grainTreeViewKeyBindings tree
    , editGid = Nothing
    }


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
