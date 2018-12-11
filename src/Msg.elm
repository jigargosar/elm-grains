module Msg exposing (Msg(..), deleteGrain, routeToGrain)

import Browser.Dom
import Grain
import GrainId exposing (GrainId)
import GrainStore
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Route exposing (Route)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result Browser.Dom.Error ())
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | AddNewClicked
    | LoadGrainStore Value
    | GrainStoreSubMsg GrainStore.Msg
    | GrainStoreReply GrainStore.Reply
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | GrainTitleChanged GrainId String


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
    routeTo <| Route.Grain gid


deleteGrain grain =
    GrainStoreSubMsg (GrainStore.deleteGrain grain)
