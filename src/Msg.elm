module Msg exposing (Msg(..))

import Browser.Dom
import GrainStore
import Random exposing (Generator)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result Browser.Dom.Error ())
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | AddNewClicked
    | GrainStoreSubMsg GrainStore.Msg
    | GrainStoreReply GrainStore.Reply
    | ToastDismiss
