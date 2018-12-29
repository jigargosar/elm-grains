module Popup exposing (Popup(..))

import GrainId exposing (GrainId)


type Popup
    = GrainMorePopup GrainId
    | GrainMovePopup GrainId
    | NoPopup
