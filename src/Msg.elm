module Msg exposing (Msg(..))

import Browser.Dom
import Random exposing (Generator)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | LogError String
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | CreateNewRequest
