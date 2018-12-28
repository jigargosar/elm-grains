module Main exposing (main, update)

import ActorId exposing (ActorId)
import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px, rgb, zero)
import CssElements exposing (..)
import CssElevation exposing (elevation)
import CssHtml
import CssIcons exposing (view)
import CssLayout exposing (flexCol, flexRow)
import CssProto
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space2, space4, white)
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
import GrainMorePopupView exposing (GrainMorePopupView)
import GrainStore exposing (GrainStore)
import GrainTreeView
import GrainView exposing (GrainView)
import GrainZipper exposing (GrainTree)
import HistoryState exposing (HistoryState)
import HotKey as K exposing (SoftKey(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Html.Styled.Keyed
import InlineEditGrain exposing (InlineEditGrain)
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
import MoveGrainPopupView exposing (MoveGrainPopupView)
import NotFoundView
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



---- MODEL ----


type Popup
    = GrainMorePopup GrainId
    | GrainMovePopup GrainId
    | NoPopup


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
                |> Random.always NoPopup
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
    { model | popup = NoPopup }


setGrainStore grainStore model =
    { model | grainStore = grainStore }



---- UPDATE ----


type GrainStoreAddMsg
    = GCAdd_After GrainId
    | GCAdd_Before GrainId
    | GCAdd_NoOp


type alias AfterGrainCreate =
    GrainStoreAddMsg


type GrainStoreMsg
    = GS_Move Direction GrainId Posix
    | GS_GrainUpdate Grain.Update GrainId Posix
    | GS_MoveOneLevelUp GrainId Posix
    | GS_MoveOneLevelDown GrainId Posix
    | GS_AddGrainAnd Grain GrainStoreAddMsg
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


type CreateGrainStep
    = CreateGrainWithNow
    | CreateGrainWithGenerator Posix
    | AddCreatedGrain Grain


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | GrainFocused GrainId Bool
      -- TOAST
    | ToastDismiss
      -- ADD GRAIN --
    | CreateGrain AfterGrainCreate CreateGrainStep
    | AddGrainClicked
    | CreateAndAddNewGrainWithNow GrainStoreAddMsg Posix
    | AddGrainToCache GrainStoreAddMsg Grain
    | AppendNewSibling GrainId
    | PrependNewSibling GrainId
      -- UPDATE GRAIN --
    | MoveGrain Direction GrainId
    | UpdateGrainStore GrainStoreMsg
    | UpdateInlineEditGrain GrainId InlineEditGrainMsg
    | DragGrain GrainId
      -- GRAIN FOCUS NAVIGATION
    | FocusRelative GrainTree GrainId FocusRelativeMsg
      -- POPUP
    | UpdatePopup PopupMsg
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


popupMsg : PopupMsg -> Msg
popupMsg =
    UpdatePopup


dismissPopupMsg : Msg
dismissPopupMsg =
    popupMsg PM_Dismiss


openPopupMsg : Popup -> Msg
openPopupMsg popup =
    popupMsg <| PM_Open popup


openGrainMovePopupMsg : GrainId -> Msg
openGrainMovePopupMsg gid =
    openPopupMsg <| GrainMovePopup gid


openGrainMorePopupMsg : GrainId -> Msg
openGrainMorePopupMsg gid =
    openPopupMsg <| GrainMorePopup gid


updatePopup msg model =
    case msg of
        PM_SetGrainParent gid parentId ->
            ( dismissPopup model
            , performGrainUpdate gid (Grain.SetParentId parentId)
            )

        PM_MoveGrain gid direction ->
            ( dismissPopup model
            , performGrainMoveInDirection direction gid
            )

        PM_SetGrainDeleted gid deleted ->
            ( dismissPopup model
            , performGrainUpdate gid (Grain.SetDeleted deleted)
            )

        PM_RouteToGrain gid ->
            update (routeToGrainTreeMsg gid) (dismissPopup model)

        PM_Dismiss ->
            dismissPopup model |> Return.singleton

        PM_Open popup ->
            Return.singleton { model | popup = popup }



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
                            performGrainSetContent gid_ content
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



-- GRAIN CACHE --


updateGrainStoreCmd msg =
    Task.perform (UpdateGrainStore << msg) Time.now


performGrainMoveInDirection direction gid =
    Task.perform (UpdateGrainStore << GS_Move direction gid) Time.now


performGrainSetContent gid content =
    performGrainUpdate gid <| Grain.SetContent content


performGrainUpdate gid grainUpdate =
    Task.perform
        (UpdateGrainStore
            << GS_GrainUpdate grainUpdate gid
        )
        Time.now


localPersistGrainStoreEffect model =
    Port.setGrainCache <| GrainStore.encoder model.grainStore


firePersistUnsavedGrainsEffect model =
    let
        dirtyGrains =
            model.grainStore
                |> GrainStore.toRawList
                |> List.filterNot SavedGrain.needsPersistence
    in
    if List.isEmpty dirtyGrains then
        Cmd.none

    else
        dirtyGrains
            |> E.list SavedGrain.encoder
            |> Port.persistSavedGrainList


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
        GS_Move direction grainId now ->
            GrainStore.move direction
                grainId
                now
                model.grainStore
                |> handleResult

        GS_GrainUpdate grainUpdate grainId now ->
            GrainStore.updateWithGrainUpdate grainUpdate
                grainId
                now
                model.grainStore
                |> handleResult

        GS_MoveOneLevelUp gid now ->
            GrainStore.moveOneLevelUp gid
                now
                model.grainStore
                |> handleResult

        GS_MoveOneLevelDown gid now ->
            GrainStore.moveOneLevelDown gid
                now
                model.grainStore
                |> handleResult

        GS_AddGrainAnd grain msg ->
            case msg of
                GCAdd_Before siblingGid ->
                    GrainStore.addNewGrainBefore siblingGid
                        grain
                        model.grainStore
                        |> handleResult

                GCAdd_After siblingGid ->
                    GrainStore.addNewAfter siblingGid
                        grain
                        model.grainStore
                        |> handleResult

                GCAdd_NoOp ->
                    GrainStore.addNew grain
                        model.grainStore
                        |> handleResult

        GS_FirebaseChanges changeList ->
            GrainStore.updateFromFirebaseChangeList changeList
                model.grainStore
                |> handleResult

        GS_Load encoded ->
            GrainStore.load encoded |> handleResult



-- URL CHANGED


updateUrlPopped : String -> Maybe HistoryState -> Model -> Return Msg Model
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



-- EXPERIMENT ELM SYNTAX / PARSER


src =
    """module Foo exposing(foo)

foo = 1
"""


parse : String -> String
parse input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed: " ++ Debug.toString e

        Ok v ->
            "Success: " ++ Debug.toString v



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

        FocusRelative tree gid msg ->
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

        AppendNewSibling gid ->
            ( model
            , performWithNow (CreateAndAddNewGrainWithNow <| GCAdd_After gid)
            )

        PrependNewSibling gid ->
            ( model
            , performWithNow (CreateAndAddNewGrainWithNow <| GCAdd_Before gid)
            )

        AddGrainClicked ->
            ( model
            , performWithNow (CreateAndAddNewGrainWithNow GCAdd_NoOp)
            )

        CreateGrain afterBuildMsg state ->
            let
                nextStep fn1 =
                    CreateGrain afterBuildMsg << fn1

                generateGrainCmd now =
                    Random.generate
                        (nextStep AddCreatedGrain)
                        (Grain.generator now)
            in
            case state of
                CreateGrainWithNow ->
                    ( model
                    , TimeX.withNow
                        (nextStep
                            CreateGrainWithGenerator
                        )
                    )

                CreateGrainWithGenerator now ->
                    ( model, generateGrainCmd now )

                AddCreatedGrain grain ->
                    updateGrainStore
                        (GS_AddGrainAnd grain afterBuildMsg)
                        model

        CreateAndAddNewGrainWithNow afterAddMsg now ->
            let
                generateTag =
                    AddGrainToCache afterAddMsg
            in
            Return.return model
                (Random.generate generateTag (Grain.generator now))

        AddGrainToCache msg grain ->
            let
                gid =
                    Grain.id grain
            in
            updateGrainStore (GS_AddGrainAnd grain msg) model
                --                |> Return.andThen
                --                    (update <| routeToGrainIdMsg <| Grain.id grain)
                |> Return.andThen (updateInlineEditGrain gid IE_Start)

        UpdateGrainStore msg ->
            updateGrainStore msg model

        UpdateInlineEditGrain gid msg ->
            updateInlineEditGrain gid msg model

        DragGrain gid ->
            Return.singleton model

        MoveGrain direction gid ->
            ( model, performGrainMoveInDirection direction gid )

        UpdatePopup msg ->
            updatePopup msg model

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
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
                |> Result.map (handleKeyDownWhenNothingIsFocused >> callWith model)
                |> handleDecodeResult model



-- VIEW --


view =
    view2


view1 _ =
    div [] [ text <| parse src ]


view2 : Model -> Html Msg
view2 model =
    let
        routeVM =
            toRouteView model.route
    in
    Skeleton.view
        { children =
            [ viewAppBar routeVM model.authState ]
                ++ viewRouteChildren model
                ++ [ viewToast model.toast
                   , viewPopup model
                   , DatGui.view
                        [ DatGui.boolean "Debug" False
                        , DatGui.integer "Counter" 100
                        ]
                   ]
        }


viewPopup model =
    case model.popup of
        GrainMorePopup gid ->
            grainById gid model
                |> Maybe.map (grainMorePopupViewModel model)
                |> CssHtml.viewMaybe GrainMorePopupView.view

        GrainMovePopup gid ->
            grainById gid model
                |> Maybe.map (moveGrainPopupViewModel model)
                |> CssHtml.viewMaybe MoveGrainPopupView.view

        NoPopup ->
            CssHtml.noView


grainMorePopupViewModel : Model -> Grain -> GrainMorePopupView Msg
grainMorePopupViewModel model grain =
    let
        deleted =
            Grain.deleted grain

        gid =
            Grain.id grain
    in
    { editMsg = popupMsg <| PM_RouteToGrain gid
    , moveUpMsg = popupMsg <| PM_MoveGrain gid Direction.Up
    , moveDownMsg = popupMsg <| PM_MoveGrain gid Direction.Down
    , moveToMsg = openGrainMovePopupMsg gid
    , toggleDeleteMsg = popupMsg <| PM_SetGrainDeleted gid (not deleted)
    , dismissMsg = dismissPopupMsg
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
    , dismissMsg = dismissPopupMsg
    , setParentMsg = popupMsg << PM_SetGrainParent gid
    , setParentToRootMsg = (popupMsg << PM_SetGrainParent gid) Grain.rootIdAsParentId
    }


type alias RouteView =
    { title : String, showBackBtn : Bool, children : List (Html Msg) }


toRouteView : Route -> RouteView
toRouteView route =
    case route of
        Route.GrainTree gid ->
            if gid == GrainId.root then
                { title = "Home", showBackBtn = False, children = [] }

            else
                { title = "Focused", showBackBtn = True, children = [] }

        Route.NotFound _ ->
            { title = "Oops!", showBackBtn = True, children = [] }


viewAppBar { title, showBackBtn } authState =
    let
        viewTitle =
            styled div
                [ CS.p2 space2 zero
                , CS.flex11Auto
                , Css.textAlign Css.center
                ]
                []
                [ text title ]

        viewBackBtn =
            button [ class "btn", onClick BackPressed ] [ text "Back" ]

        viewAuthState =
            case authState of
                Firebase.AuthStateLoading ->
                    button [ class "btn loading" ] [ text "SignIn" ]

                Firebase.AuthStateUser user ->
                    button [ class "btn", onClick SignOut ] [ text "SignOut" ]

                Firebase.AuthStateNoUser ->
                    button [ class "btn", onClick SignIn ] [ text "SignIn" ]
    in
    CssLayout.flexRow
        [ CS.sticky
        , Css.top <| px 0
        , CS.p2 zero space2
        , CS.itemsCenter
        ]
        [ class "bg-dark" ]
        [ CssHtml.viewIf showBackBtn viewBackBtn
        , viewTitle
        , viewAuthState
        ]


grainTreeViewConfig tree =
    let
        treeRootGid =
            Tree.label tree
                |> Grain.id

        frParentMsg gid =
            FocusRelative tree gid FR_Parent

        arrowLeftMsg gid =
            if gid == treeRootGid && gid /= GrainId.root then
                BackPressed

            else
                frParentMsg gid
    in
    { focusRouteTo = routeToGrainTreeMsg
    , keyDownCustom =
        \gid ->
            let
                fr =
                    FocusRelative tree gid

                moveGrain direction =
                    MoveGrain direction gid

                moveGrainPD direction =
                    pd <| moveGrain direction

                moveMappings =
                    List.map
                        (Tuple2.double
                            >> Tuple.mapBoth
                                K.metaArrow
                                moveGrain
                        )
                        Direction.list

                bindings =
                    [ ( K.arrowDown, fr FR_Forward )
                    , ( K.arrowUp, fr FR_Backward )
                    , ( K.arrowLeft, arrowLeftMsg gid )
                    , ( K.arrowRight, routeToGrainTreeMsg gid )
                    ]
                        ++ moveMappings
                        |> List.map (Tuple.mapSecond pd)
            in
            K.bindEachToMsg bindings
    }


viewGrainTreeById gid model =
    let
        viewTree grainTree =
            GrainTreeView.view (grainTreeViewConfig grainTree) grainTree
    in
    model.grainStore
        |> GrainStore.treeFromGid gid
        |> Maybe.unwrap NotFoundView.view viewTree


viewRouteChildren model =
    case model.route of
        Route.GrainTree gid ->
            viewGrainTreeById gid model

        Route.NotFound string ->
            NotFoundView.view


viewToast toast =
    Toast.view ToastDismiss toast


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
