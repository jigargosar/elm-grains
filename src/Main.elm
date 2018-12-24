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
import EventX exposing (onKeyDownPD)
import FireUser exposing (FireUser)
import Firebase
import Grain exposing (Grain)
import GrainCache exposing (GrainCache)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainListView exposing (GrainListView)
import GrainMorePopupView exposing (GrainMorePopupView)
import GrainView exposing (GrainView)
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
import Tuple exposing (mapFirst)



---- MODEL ----


type Popup
    = GrainMorePopup GrainId
    | MoveGrainPopup GrainId
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
                |> Random.always GrainCache.empty
                |> Random.finish
    in
    model
        |> update (UpdateGrainCache <| LoadGrainCache flags.grainCache)


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
    .grainCache >> GrainCache.get gid


setNewSeed newSeed model =
    { model | seed = newSeed }


dismissPopup model =
    { model | popup = NoPopup }


setGrainCache grainCache model =
    { model | grainCache = grainCache }



---- UPDATE ----


type GrainCacheMsg
    = MoveGrainBy GrainId Int Posix
    | GrainUpdate GrainId Grain.Update Posix
    | AddGrain Grain
    | FirebaseChanges (List GrainChange)
    | LoadGrainCache Value


type InlineEditGrainMsg
    = IE_Start
    | IE_Content String
    | IE_Submit


type PopupMsg
    = PA_SetGrainParent GrainId Grain.ParentId
    | PA_MoveGrain GrainId Int
    | PA_SetGrainDeleted GrainId Bool
    | PA_RouteToGrain GrainId


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | ShowMoveToPopup GrainId
    | UpdateGrainCache GrainCacheMsg
    | PopupAction PopupMsg Bool
    | DismissPopup
    | GrainMoreClicked GrainId
    | DragGrain GrainId
    | AddGrainClicked
    | CreateAndAddNewGrainWithNow Posix
    | AddGrainToCache Grain
    | BackPressed
    | GrainContentChanged GrainId String
    | UpdateInlineEditGrain GrainId InlineEditGrainMsg
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | ErrorString String
    | SignIn
    | SignOut


routeTo route =
    RouteTo route


routeToGrainIdMsg gid =
    routeTo <| Route.Grain gid


autoFocusRoute route =
    let
        maybeDomId =
            case route of
                Route.Grain _ ->
                    Just GrainView.autoFocusId

                _ ->
                    Nothing
    in
    maybeDomId |> unwrapMaybeCmd (BrowserX.focus FocusResult)


unwrapMaybeCmd cmdFn =
    Maybe.unwrap Cmd.none cmdFn


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Port.urlChanged UrlChanged
        , Port.fire2Elm Firebase
        ]


handleErrorString : String -> Model -> ( Model, Cmd Msg )
handleErrorString errString model =
    Return.return (mapToast (Toast.show errString) model)
        (Port.error errString)



-- POPUP UPDATE


popupMsg : PopupMsg -> Msg
popupMsg action =
    PopupAction action False



-- UPDATE InlineEditGrain --


updateInlineEditGrain gid msg model =
    case msg of
        IE_Start ->
            let
                inlineEdit grain =
                    Return.singleton
                        { model
                            | inlineEditGrain = InlineEditGrain.startEditing grain
                        }
                        |> Return.command
                            (BrowserX.focus FocusResult <|
                                GrainListView.inlineGrainEditInputDomId grain
                            )
            in
            grainById gid model
                |> Maybe.unwrap (Return.singleton model)
                    inlineEdit

        IE_Content content ->
            case InlineEditGrain.onContentChange content model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok inlineEditGrain ->
                    Return.singleton
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }

        IE_Submit ->
            case InlineEditGrain.endEditing model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok ( gid_, content, inlineEditGrain ) ->
                    Return.return
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }
                        (performGrainUpdate gid_ (Grain.SetContent content))



-- GRAIN CACHE --


performGrainMove gid offset =
    Task.perform (UpdateGrainCache << MoveGrainBy gid offset) Time.now


performGrainUpdate gid grainUpdate =
    Task.perform (UpdateGrainCache << GrainUpdate gid grainUpdate) Time.now


localPersistGrainCacheEffect model =
    Port.setGrainCache <| GrainCache.encoder model.grainCache


firePersistUnsavedGrainsEffect model =
    let
        dirtyGrains =
            model.grainCache
                |> GrainCache.toList
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

        handleResult result =
            result
                |> Result.mapBoth handleErrorString setGrainCacheAndPersist
                |> Result.merge
                |> callWith model
    in
    case message of
        MoveGrainBy grainId offset now ->
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

        AddGrain grain ->
            GrainCache.addNewGrain grain
                model.grainCache
                |> handleResult

        FirebaseChanges changeList ->
            GrainCache.updateFromFirebaseChangeList changeList
                model.grainCache
                |> handleResult

        LoadGrainCache encoded ->
            GrainCache.load encoded |> handleResult



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

        ErrorString errString ->
            handleErrorString errString model

        ToastDismiss ->
            Return.singleton (mapToast Toast.dismiss model)

        FocusResult (Ok ()) ->
            Return.singleton model

        FocusResult (Err errorString) ->
            handleErrorString errorString model

        GrainContentChanged gid content ->
            ( model
            , performGrainUpdate gid (Grain.SetContent content)
            )

        UpdateInlineEditGrain gid msg ->
            updateInlineEditGrain gid msg model

        ShowMoveToPopup gid ->
            Return.singleton { model | popup = MoveGrainPopup gid }

        PopupAction msg dismiss ->
            case msg of
                PA_SetGrainParent gid parentId ->
                    ( dismissPopup model
                    , performGrainUpdate gid (Grain.SetParentId parentId)
                    )

                PA_MoveGrain gid offset ->
                    ( dismissPopup model
                    , performGrainMove gid offset
                    )

                PA_SetGrainDeleted gid deleted ->
                    ( dismissPopup model
                    , performGrainUpdate gid (Grain.SetDeleted deleted)
                    )

                PA_RouteToGrain gid ->
                    update (routeToGrainIdMsg gid) (dismissPopup model)

        DismissPopup ->
            dismissPopup model |> Return.singleton

        GrainMoreClicked gid ->
            Return.singleton { model | popup = GrainMorePopup gid }

        DragGrain gid ->
            Return.singleton model

        UpdateGrainCache msg ->
            updateGrainCache msg model

        AddGrainClicked ->
            ( model
            , performWithNow CreateAndAddNewGrainWithNow
            )

        CreateAndAddNewGrainWithNow now ->
            Return.return model
                (Random.generate AddGrainToCache (Grain.generator now))

        AddGrainToCache grain ->
            updateGrainCache (AddGrain grain) model
                |> Return.andThen
                    (update <| routeToGrainIdMsg <| Grain.id grain)

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
                |> Return.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            Return.singleton (setRouteFromString url model)

        Firebase encodedMsg ->
            case Firebase.decodeInbound encodedMsg of
                Firebase.Error errString ->
                    handleErrorString errString model

                Firebase.AuthStateChanged authState ->
                    setAuthState authState model
                        |> Return.singleton

                Firebase.GrainChanges changes ->
                    updateGrainCache (FirebaseChanges changes) model

        SignIn ->
            Return.return model (Firebase.signIn ())

        SignOut ->
            Return.return model (Firebase.signOut ())

        BackPressed ->
            Return.return model (Port.navigateBack ())


performWithNow nowToMsg =
    Task.perform nowToMsg Time.now


view : Model -> Html Msg
view model =
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

        MoveGrainPopup gid ->
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
    { editMsg = popupMsg <| PA_RouteToGrain gid
    , moveUpMsg = popupMsg <| PA_MoveGrain gid -1
    , moveDownMsg = popupMsg <| PA_MoveGrain gid 1
    , moveToMsg = ShowMoveToPopup gid
    , toggleDeleteMsg = popupMsg <| PA_SetGrainDeleted gid (not deleted)
    , dismissMsg = DismissPopup
    , deleted = deleted
    }


moveGrainPopupViewModel : Model -> Grain -> MoveGrainPopupView Msg
moveGrainPopupViewModel model grain =
    let
        gid =
            Grain.id grain
    in
    { grain = grain
    , otherGrains =
        model.grainCache
            |> GrainCache.toList
            |> List.map SavedGrain.value
            |> List.filterNot
                (GrainCache.isDescendent
                    >> callWith2 grain model.grainCache
                )
    , isSelected = Grain.isParentOf grain
    , dismissMsg = DismissPopup
    , setParentMsg = popupMsg << PA_SetGrainParent gid
    , setParentToRootMsg = (popupMsg << PA_SetGrainParent gid) Grain.rootParentId
    }


type alias RouteView =
    { title : String, showBackBtn : Bool, children : List (Html Msg) }


toRouteView : Route -> RouteView
toRouteView route =
    case route of
        Route.GrainList ->
            { title = "Grain List", showBackBtn = False, children = [] }

        Route.Grain _ ->
            { title = "Grain", showBackBtn = True, children = [] }

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


viewRouteChildren model =
    case model.route of
        Route.GrainList ->
            toGrainListView model |> GrainListView.view

        Route.Grain gid ->
            let
                viewModel : Maybe Grain -> GrainView Msg
                viewModel =
                    Maybe.map
                        (\grain ->
                            { contentChangedMsg =
                                GrainContentChanged (Grain.id grain)
                            , content = Grain.content grain
                            }
                        )
            in
            grainById gid model
                |> viewModel
                |> GrainView.view

        Route.NotFound string ->
            NotFoundView.view


viewToast toast =
    Toast.view ToastDismiss toast


toGrainListView : Model -> GrainListView Msg
toGrainListView model =
    let
        sort =
            List.sortWith Grain.defaultComparator

        allGrains =
            model.grainCache
                |> GrainCache.toList
                |> List.map SavedGrain.value
                |> sort

        rootGrains =
            allGrains |> List.filter (Grain.parentIdEq Grain.rootParentId)

        modifiedAtDesc =
            Grain.modifiedAt >> Time.posixToMillis >> negate

        createdAtAtDesc =
            Grain.createdAt >> Time.posixToMillis >> negate
    in
    { grains = rootGrains
    , getChildren = \parent -> List.filter (Grain.isChildOf parent) allGrains
    , inlineEditGrain = model.inlineEditGrain
    , addFabClicked = AddGrainClicked
    , grainMsg =
        { grainMoreClicked = GrainMoreClicked
        , inlineEditGrain = \gid -> UpdateInlineEditGrain gid IE_Start
        , dragGrain = DragGrain
        , inlineEditGrainContentChanged =
            \gid -> UpdateInlineEditGrain gid << IE_Content
        , inlineEditSubmit = \gid -> UpdateInlineEditGrain gid IE_Submit
        }
    }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
