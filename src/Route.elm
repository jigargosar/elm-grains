module Route exposing (..)

import GrainId exposing (GrainId)

type Route =
  GrainList
  | Grain GrainId

