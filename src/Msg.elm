module Msg exposing
    ( Msg(..)
    , UpdateGrainMsg(..)
    , routeToGrain
    )

import Browser.Dom
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainStore exposing (GrainStore)
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Route exposing (Route)
import Time exposing (Posix)


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
    | RestoreGrain Grain
    | DeleteGrain Grain
    | GrainMoreAction Msg
    | ShowMoveToPopup Grain
    | DismissPopup
    | UpgradeGrainWithNow GrainId UpdateGrainMsg Posix
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
    | InlineEditGrainSubmit
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
