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
import GrainStore exposing (GrainStore)
import GrainView
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
    { grainStore : GrainStore
    , toast : Toast
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
                |> Random.always GrainStore.empty
                |> Random.always Toast.init
                |> Random.always (Route.fromString flags.url)
                |> Random.always Firebase.initialAuthState
                |> Random.with ActorId.generator
                |> Random.always NoPopup
                |> Random.always InlineEditGrain.initialValue
                |> Random.always GrainCache.empty
                |> Random.finish
    in
    update (LoadGrainStore flags.grains) model
        |> Return.andThen (update (LoadGrainCache flags.grainCache))


setGrainStore grainStore model =
    { model | grainStore = grainStore }


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


mapGrainStore fn model =
    { model | grainStore = fn model.grainStore }


setNewSeed newSeed model =
    { model | seed = newSeed }


dismissPopup model =
    { model | popup = NoPopup }


setGrainCache grainCache model =
    { model | grainCache = grainCache }



---- UPDATE ----


type UpdateGrainMsg
    = SetGrainContent String
    | SetGrainDeleted Bool
    | SetGrainParentId Grain.ParentId
    | MoveGrainBy Int


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | LoadGrainStore Value
    | LoadGrainCache Value
    | RestoreGrain Grain
    | DeleteGrain Grain
    | GrainMoreAction Msg
    | ShowMoveToPopup Grain
    | DismissPopup
    | UpdateGrainWithNow GrainId UpdateGrainMsg Posix
    | PopupActionSetGrainParent Grain Grain.ParentId
    | PopupActionMoveGrainUp Grain
    | PopupActionMoveGrainDown Grain
    | GrainMoreClicked Grain
    | DragGrain Grain
    | CreateAndAddNewGrain
    | CreateAndAddNewGrainWithNow Posix
    | AddNewGrain Grain
    | BackPressed
    | InlineEditGrain Grain
    | InlineEditGrainSubmit Grain
    | GrainContentChanged Grain String
    | InlineEditGrainContentChanged Grain String
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | ErrorString String
    | SignIn
    | SignOut


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
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


decodeValueAndHandleError { decoder, value, onOk } model =
    D.decodeValue decoder value
        |> Result.mapError
            (D.errorToString
                >> handleErrorString
                >> callWith model
            )
        |> Result.map (onOk >> callWith model)
        |> Result.merge


addNewGrainToCache grain model =
    GrainCache.addNewGrain grain model.grainCache
        |> Result.mapBoth handleErrorString setGrainCacheAndLocalPersist
        |> Result.merge
        |> callWith model


updateGrainCacheFromFirebaseChanges :
    List GrainChange
    -> Model
    -> ( Model, Cmd Msg )
updateGrainCacheFromFirebaseChanges changeList model =
    let
        handleChange change =
            let
                grain =
                    GrainChange.grain change
            in
            case GrainChange.type_ change of
                GrainChange.Added ->
                    GrainCache.setSaved grain

                GrainChange.Modified ->
                    GrainCache.setSaved grain

                GrainChange.Removed ->
                    GrainCache.remove grain

        grainCache =
            List.foldr handleChange model.grainCache changeList
    in
    setGrainCacheAndLocalPersist grainCache model


type UpdateGrainCacheMsg
    = UpdateWithGrainMsg Grain.Update
    | UpdateGrainMoveBy Int


updateGrainCacheWithNow :
    GrainId
    -> UpdateGrainMsg
    -> Posix
    -> Model
    -> ( Model, Cmd Msg )
updateGrainCacheWithNow gid message now model =
    let
        updateGrainCacheMsg =
            case message of
                SetGrainContent content ->
                    UpdateWithGrainMsg <| Grain.SetContent content

                SetGrainDeleted deleted ->
                    UpdateWithGrainMsg <| Grain.SetDeleted deleted

                SetGrainParentId parentId ->
                    UpdateWithGrainMsg <| Grain.SetParentId parentId

                MoveGrainBy offset ->
                    UpdateGrainMoveBy offset
    in
    case updateGrainCacheMsg of
        UpdateWithGrainMsg grainUpdateMsg ->
            GrainCache.updateWithGrainMsg now grainUpdateMsg gid model.grainCache
                |> Result.mapBoth handleErrorString setGrainCacheAndLocalPersist
                |> Result.merge
                |> callWith model

        UpdateGrainMoveBy offset ->
            GrainCache.moveBy offset now gid model.grainCache
                |> Result.mapBoth handleErrorString setGrainCacheAndLocalPersist
                |> Result.merge
                |> callWith model


setGrainCacheAndLocalPersist grainCache model =
    let
        cacheCmd =
            Port.setGrainCache <| GrainCache.encoder grainCache
    in
    ( setGrainCache grainCache model, cacheCmd )


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

        GrainContentChanged grain content ->
            ( model
            , updateGrainWithNowCmd grain (SetGrainContent content)
            )

        InlineEditGrainContentChanged grain content ->
            case InlineEditGrain.onContentChange content model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok inlineEditGrain ->
                    Return.singleton
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }

        InlineEditGrain grain ->
            Return.singleton
                { model
                    | inlineEditGrain = InlineEditGrain.startEditing grain
                }
                |> Return.command
                    (BrowserX.focus FocusResult <|
                        GrainListView.inlineGrainEditInputDomId grain
                    )

        InlineEditGrainSubmit grain ->
            case InlineEditGrain.endEditing model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok ( gid, content, inlineEditGrain ) ->
                    Return.return
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }
                        (updateGrainIdWithNowCmd gid (SetGrainContent content))

        DeleteGrain grain ->
            ( model
            , updateGrainWithNowCmd grain (SetGrainDeleted True)
            )

        RestoreGrain grain ->
            ( model
            , updateGrainWithNowCmd grain (SetGrainDeleted False)
            )

        GrainMoreAction msg ->
            update msg model
                |> Return.map dismissPopup

        ShowMoveToPopup grain ->
            Return.singleton { model | popup = GrainMovePopup (Grain.id grain) }

        PopupActionSetGrainParent grain parentId ->
            ( dismissPopup model
            , updateGrainWithNowCmd grain (SetGrainParentId parentId)
            )

        PopupActionMoveGrainUp grain ->
            ( dismissPopup model
            , updateGrainWithNowCmd grain (MoveGrainBy -1)
            )

        PopupActionMoveGrainDown grain ->
            ( dismissPopup model
            , updateGrainWithNowCmd grain (MoveGrainBy 1)
            )

        DismissPopup ->
            dismissPopup model
                |> Return.singleton

        GrainMoreClicked grain ->
            Return.singleton { model | popup = GrainMorePopup (Grain.id grain) }

        DragGrain grain ->
            Return.singleton model

        UpdateGrainWithNow gid msg now ->
            let
                updateGrainHelp fn =
                    case fn now gid model.grainStore of
                        Err errString ->
                            handleErrorString errString model

                        Ok ( newGrainStore, cmd ) ->
                            Return.return (setGrainStore newGrainStore model) cmd
            in
            (case msg of
                SetGrainContent content ->
                    GrainStore.setContent content

                SetGrainDeleted deleted ->
                    GrainStore.setDeleted deleted

                SetGrainParentId parentId ->
                    GrainStore.setParentId parentId

                MoveGrainBy offset ->
                    GrainStore.moveBy offset
            )
                |> updateGrainHelp
                |> Return.andThen (updateGrainCacheWithNow gid msg now)

        CreateAndAddNewGrain ->
            ( model
            , performWithNow CreateAndAddNewGrainWithNow
            )

        CreateAndAddNewGrainWithNow now ->
            Return.return model
                (Random.generate AddNewGrain (Grain.generator now))

        AddNewGrain grain ->
            let
                return =
                    case GrainStore.addNewGrain grain model.grainStore of
                        Err errString ->
                            handleErrorString errString model

                        Ok ( newGrainStore, cmd ) ->
                            Return.return (setGrainStore newGrainStore model) cmd
            in
            return
                |> Return.andThen (addNewGrainToCache grain)
                |> Return.andThen (update (routeToGrain grain))

        LoadGrainStore val ->
            let
                ( newGrainStore, cmd ) =
                    GrainStore.loadCache val model.grainStore
            in
            Return.return (setGrainStore newGrainStore model) cmd

        LoadGrainCache encoded ->
            decodeValueAndHandleError
                { decoder = GrainCache.decoder
                , value = encoded
                , onOk =
                    \grainCache ->
                        setGrainCache grainCache >> Return.singleton
                }
                model

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
                |> Return.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            Return.singleton (setRouteFromString url model)

        Firebase encodedMsg ->
            let
                handleFireMsg fireMsg =
                    case fireMsg of
                        Firebase.Error errString ->
                            handleErrorString errString model

                        Firebase.AuthStateChanged authState ->
                            Return.singleton (setAuthState authState model)

                        Firebase.GrainChanges changes ->
                            let
                                ( newGrainStore, cmd ) =
                                    GrainStore.onFirebaseChanges changes
                                        model.grainStore
                            in
                            Return.return (setGrainStore newGrainStore model) cmd
                                |> Return.andThen
                                    (updateGrainCacheFromFirebaseChanges changes)
            in
            handleFireMsg (Firebase.decodeInbound encodedMsg)

        SignIn ->
            Return.return model (Firebase.signIn ())

        SignOut ->
            Return.return model (Firebase.signOut ())

        BackPressed ->
            Return.return model (Port.navigateBack ())


performWithNow nowToMsg =
    Task.perform nowToMsg Time.now


updateGrainWithNowCmd grain msg =
    updateGrainIdWithNowCmd (Grain.id grain) msg


updateGrainIdWithNowCmd gid msg =
    Task.perform (UpdateGrainWithNow gid msg) Time.now


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
                |> CssHtml.viewMaybe viewGrainMorePopup

        GrainMovePopup gid ->
            grainById gid model
                |> Maybe.map (grainMovePopupViewModel model)
                |> CssHtml.viewMaybe viewGrainMovePopup

        NoPopup ->
            CssHtml.noView


grainMovePopupViewModel model grain =
    { grain = grain
    , otherGrains =
        model.grainCache
            |> GrainCache.toList
            |> List.map SavedGrain.value
            |> List.filterNot
                (GrainCache.isDescendent
                    >> callWith2 grain model.grainCache
                )
    }


viewGrainMovePopup { grain, otherGrains } =
    let
        viewGrainItem g =
            let
                isCurrentParent =
                    Grain.isParentOf grain g
            in
            flexCol
                [ CS.pointer
                , CS.styleIf isCurrentParent CS.bold
                , Css.hover
                    [ Css.property "background-color" "lightgray"
                    ]
                ]
                [ onClick <|
                    PopupActionSetGrainParent grain
                        (Grain.idAsParentId g)
                ]
                [ flexRow [ CS.ellipsis ]
                    []
                    [ text <| Grain.titleOrEmpty g
                    ]
                ]

        viewRootItem =
            let
                isRoot =
                    Grain.parentIdEq Grain.isRoot grain
            in
            flexCol
                [ CS.pointer
                , CS.styleIf isRoot CS.bold
                , Css.hover
                    [ Css.property "background-color" "lightgray"
                    ]
                ]
                [ onClick <|
                    PopupActionSetGrainParent grain
                        Grain.isRoot
                ]
                [ flexRow [ CS.ellipsis ]
                    []
                    [ text "<Root>"
                    ]
                ]
    in
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Move Grain" ]
                , flexCol []
                    []
                    (viewRootItem
                        :: List.map viewGrainItem otherGrains
                    )
                ]
            ]
        , onDismiss = DismissPopup
        }


viewGrainMorePopup grain =
    let
        viewEdit : Grain -> Html Msg
        viewEdit g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (GrainMoreAction <| routeToGrain g) ]
                [ flexCol [] [] [ text "Edit" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.modeEdit
                    ]
                ]

        viewMoveUp g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (PopupActionMoveGrainUp g) ]
                [ flexCol [] [] [ text "Move Up" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowUp
                    ]
                ]

        viewMoveDown g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (PopupActionMoveGrainDown g) ]
                [ flexCol [] [] [ text "Move Down" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowDown
                    ]
                ]

        viewNestUnder g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (ShowMoveToPopup g) ]
                [ flexCol [] [] [ text "Nest Under..." ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.dragHandle
                    ]
                ]

        viewDelete g =
            let
                deleted =
                    Grain.deleted g

                action =
                    (ter deleted RestoreGrain DeleteGrain <|
                        g
                    )
                        |> GrainMoreAction

                actionTitle =
                    ter deleted "Restore" "Trash"

                icon =
                    ter deleted CssIcons.restore CssIcons.delete
            in
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick action ]
                [ flexCol [] [] [ text actionTitle ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view icon
                    ]
                ]
    in
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Grain Menu" ]
                , viewEdit grain
                , viewMoveUp grain
                , viewMoveDown grain
                , viewNestUnder grain
                , viewDelete grain
                ]
            ]
        , onDismiss = DismissPopup
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
            grainById gid model |> GrainView.view GrainContentChanged

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
            allGrains |> List.filter (Grain.parentIdEq Grain.isRoot)

        modifiedAtDesc =
            Grain.modifiedAt >> Time.posixToMillis >> negate

        createdAtAtDesc =
            Grain.createdAt >> Time.posixToMillis >> negate
    in
    { grains = rootGrains
    , getChildren = \parent -> List.filter (Grain.isChildOf parent) allGrains
    , inlineEditGrain = model.inlineEditGrain
    , addFabClicked = CreateAndAddNewGrain
    , grainMsg =
        { grainMoreClicked = GrainMoreClicked
        , inlineEditGrain = InlineEditGrain
        , dragGrain = DragGrain
        , inlineEditGrainContentChanged = InlineEditGrainContentChanged
        , inlineEditSubmit = InlineEditGrainSubmit
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
