module Msg exposing (Msg(..))

import Browser.Dom
import GrainStore
import Random exposing (Generator)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | LogError String
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | AddNewClicked
    | GrainStoreSub GrainStore.Msg
    | GrainStoreReply GrainStore.Reply
