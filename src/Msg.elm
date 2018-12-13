module Msg exposing
    ( Msg(..)
    , addNewGrainClicked
    , deleteGrain
    , grainContentChanged
    , routeToGrain
    )

import Browser.Dom
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainStore
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Route exposing (Route)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | LoadGrainStore Value
    | GrainStoreSubMsg GrainStore.Msg
    | GrainStoreReply GrainStore.Reply
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | AuthUser
    | AuthUserNone


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
    routeTo <| Route.Grain gid


deleteGrain grain =
    GrainStoreSubMsg (GrainStore.deleteGrain grain)


grainContentChanged grain content =
    GrainStoreSubMsg (GrainStore.setContent grain content)


addNewGrainClicked =
    GrainStoreSubMsg GrainStore.createNewGrain
