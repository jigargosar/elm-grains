module Msg exposing (Msg(..))

import Browser.Dom
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
