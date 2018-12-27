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
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import Elm.Parser
import EventX exposing (onKeyDownPD, pNone, pd, sp)
import FireUser exposing (FireUser)
import Firebase
import Grain exposing (Grain)
import GrainCache exposing (GrainCache)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainMorePopupView exposing (GrainMorePopupView)
import GrainTreeView
import GrainView exposing (GrainView)
import GrainZipper exposing (GrainTree)
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
import SavedGrain
import Skeleton
import Tagged
import Task
import Time exposing (Posix)
import TimeX
import Toast exposing (Toast)
import Tree
import Tuple exposing (mapFirst)
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
    , grainCache : GrainCache
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
                |> Random.always GrainCache.init
                |> Random.always Nothing
                |> Random.always Nothing
                |> Random.finish
    in
    model
        |> update (UpdateGrainCache <| GC_Load flags.grainCache)


setRoute route model =
    { model | route = route }


setRouteFromString =
    Route.fromString >> setRoute


mapToast fn model =
    { model | toast = fn model.toast }


setAuthState authState model =
    { model | authState = authState }


grainById gid =
    savedGrainById gid >> Maybe.map SavedGrain.value


savedGrainById gid =
    .grainCache >> GrainCache.get__ gid


setNewSeed newSeed model =
    { model | seed = newSeed }


dismissPopup model =
    { model | popup = NoPopup }


setGrainCache grainCache model =
    { model | grainCache = grainCache }



---- UPDATE ----


type GrainCacheAddMsg
    = GCAdd_After GrainId
    | GCAdd_Before GrainId
    | GCAdd_NoOp


type GrainCacheMsg
    = GC_MoveBy GrainId Int Posix
    | GrainUpdate GrainId Grain.Update Posix
    | GC_MoveOneLevelUp GrainId Posix
    | GC_MoveOneLevelDown GrainId Posix
    | GC_AddGrainAnd Grain GrainCacheAddMsg
    | GC_FirebaseChanges (List GrainChange)
    | GC_Load Value


type InlineEditGrainMsg
    = IE_Start
    | IE_Content String
    | IE_Submit
    | IE_Discard
    | IE_KeyboardFocus Bool


type PopupMsg
    = PM_SetGrainParent GrainId Grain.ParentId
    | PM_MoveGrain GrainId Int
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
    | AddGrainClicked
    | CreateAndAddNewGrainWithNow GrainCacheAddMsg Posix
    | AddGrainToCache GrainCacheAddMsg Grain
    | AppendNewSibling GrainId
    | PrependNewSibling GrainId
      -- UPDATE GRAIN --
    | MoveGrainBy GrainId Int
    | MoveGrainOneLevelUp GrainId
    | MoveGrainOneLevelDown GrainId
    | UpdateGrainCache GrainCacheMsg
    | UpdateInlineEditGrain GrainId InlineEditGrainMsg
    | DragGrain GrainId
      -- GRAIN FOCUS NAVIGATION
    | FocusRelative GrainId GrainTree FocusRelativeMsg
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


handleError : String -> Model -> ( Model, Cmd Msg )
handleError errString model =
    Return.return (mapToast (Toast.show errString) model)
        (Port.error errString)


handleStringResult :
    Model
    -> Result String (Return Msg Model)
    -> Return Msg Model
handleStringResult model =
    Result.mapError
        (handleError >> callWith model)
        >> Result.merge


handleDecodeResult :
    Model
    -> Result D.Error (Return Msg Model)
    -> Return Msg Model
handleDecodeResult model =
    Result.mapError
        (D.errorToString
            >> handleError
            >> callWith model
        )
        >> Result.merge


performWithNow nowToMsg =
    Task.perform nowToMsg Time.now


focusCmd =
    BrowserX.focus FocusResult


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

        PM_MoveGrain gid offset ->
            ( dismissPopup model
            , performGrainMove gid offset
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


updateGrainCacheCmd msg =
    Task.perform (UpdateGrainCache << msg) Time.now


performGrainMove gid offset =
    Task.perform (UpdateGrainCache << GC_MoveBy gid offset) Time.now


performGrainSetContent gid content =
    performGrainUpdate gid <| Grain.SetContent content


performGrainUpdate gid grainUpdate =
    Task.perform (UpdateGrainCache << GrainUpdate gid grainUpdate) Time.now


localPersistGrainCacheEffect model =
    Port.setGrainCache <| GrainCache.encoder model.grainCache


firePersistUnsavedGrainsEffect model =
    let
        dirtyGrains =
            model.grainCache
                |> GrainCache.toRawList
                |> List.filterNot SavedGrain.saved
    in
    if List.isEmpty dirtyGrains then
        Cmd.none

    else
        dirtyGrains
            |> E.list SavedGrain.encoder
            |> Port.persistSavedGrainList


updateGrainCache :
    GrainCacheMsg
    -> Model
    -> ( Model, Cmd Msg )
updateGrainCache message model =
    let
        setGrainCacheAndPersist newGrainCache =
            Return.singleton
                >> Return.map (setGrainCache newGrainCache)
                >> Return.effect_ localPersistGrainCacheEffect
                >> Return.effect_ firePersistUnsavedGrainsEffect

        handleResult =
            Result.map (setGrainCacheAndPersist >> callWith model)
                >> handleStringResult model
    in
    case message of
        GC_MoveBy grainId offset now ->
            GrainCache.moveBy offset
                grainId
                now
                model.grainCache
                |> handleResult

        GrainUpdate grainId grainUpdate now ->
            GrainCache.updateWithGrainUpdate grainUpdate
                grainId
                now
                model.grainCache
                |> handleResult

        GC_MoveOneLevelUp gid now ->
            GrainCache.moveOneLevelUp gid
                now
                model.grainCache
                |> handleResult

        GC_MoveOneLevelDown gid now ->
            GrainCache.moveOneLevelDown gid
                now
                model.grainCache
                |> handleResult

        GC_AddGrainAnd grain msg ->
            case msg of
                GCAdd_Before siblingGid ->
                    GrainCache.addNewGrainBefore siblingGid
                        grain
                        model.grainCache
                        |> handleResult

                GCAdd_After siblingGid ->
                    GrainCache.addNewAfter siblingGid
                        grain
                        model.grainCache
                        |> handleResult

                GCAdd_NoOp ->
                    GrainCache.addNew grain
                        model.grainCache
                        |> handleResult

        GC_FirebaseChanges changeList ->
            GrainCache.updateFromFirebaseChangeList changeList
                model.grainCache
                |> handleResult

        GC_Load encoded ->
            GrainCache.load encoded |> handleResult



-- URL CHANGED


updateUrlPopped : String -> Model -> Return Msg Model
updateUrlPopped url model =
    let
        newRoute =
            Route.fromString url

        oldRoute =
            model.route

        maybeFocusDomId : Maybe String
        maybeFocusDomId =
            case oldRoute of
                Route.GrainTree gid ->
                    Just <| GrainTreeView.grainDomId gid

                Route.NotFound _ ->
                    Nothing

        focusEffect newModel =
            maybeFocusDomId
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
        updateUrlPopped (UrlChange.url event) model

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

        FocusRelative gid tree msg ->
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
            updateGrainCache (GC_AddGrainAnd grain msg) model
                --                |> Return.andThen
                --                    (update <| routeToGrainIdMsg <| Grain.id grain)
                |> Return.andThen (updateInlineEditGrain gid IE_Start)

        UpdateGrainCache msg ->
            updateGrainCache msg model

        UpdateInlineEditGrain gid msg ->
            updateInlineEditGrain gid msg model

        DragGrain gid ->
            Return.singleton model

        MoveGrainBy gid offset ->
            ( model, performGrainMove gid offset )
                |> Return.command (focusGidCmd gid)

        MoveGrainOneLevelUp gid ->
            ( model, updateGrainCacheCmd <| GC_MoveOneLevelUp gid )
                |> Return.command (focusGidCmd gid)

        MoveGrainOneLevelDown gid ->
            ( model, updateGrainCacheCmd <| GC_MoveOneLevelDown gid )
                |> Return.command (focusGidCmd gid)

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
                    handleError errString model

                Firebase.AuthStateChanged authState ->
                    setAuthState authState model
                        |> Return.singleton

                Firebase.GrainChanges changes ->
                    updateGrainCache (GC_FirebaseChanges changes) model

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
    , moveUpMsg = popupMsg <| PM_MoveGrain gid -1
    , moveDownMsg = popupMsg <| PM_MoveGrain gid 1
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
            GrainCache.rejectSubTreeAndFlatten grain model.grainCache
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
    { focusRouteTo = routeToGrainTreeMsg
    , keyDownCustom =
        \gid ->
            let
                frPD =
                    pd << FocusRelative gid tree
            in
            K.bindEachToMsg
                [ ( K.arrowDown, frPD FR_Forward )
                , ( K.arrowUp, frPD FR_Backward )
                , ( K.arrowLeft, frPD FR_Parent )
                , ( K.arrowRight, pd <| routeToGrainTreeMsg gid )
                ]
    }


viewGrainTreeById gid model =
    let
        viewTree grainTree =
            GrainTreeView.view (grainTreeViewConfig grainTree) grainTree
    in
    model.grainCache
        |> GrainCache.treeFromGid gid
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
