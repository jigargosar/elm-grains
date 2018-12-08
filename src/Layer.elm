module Layer exposing (Layer(..))

import AddLayer exposing (AddLayer(..))
import CmdPalette exposing (CmdPalette)
import GrainFilter exposing (GrainFilter)
import SList exposing (SList)


type Layer
    = Base
    | Add AddLayer
    | CmdPalette CmdPalette
