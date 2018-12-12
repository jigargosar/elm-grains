module Revision exposing (Revision)


type alias Model =
    { version : Int
    }


type Revision
    = Revision Model


init =
    Revision { version = 0 }


unwrap (Revision model) =
    model


map fn =
    unwrap >> fn >> Revision
