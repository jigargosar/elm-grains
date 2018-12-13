module FireUser exposing (FireUser)


type alias Model =
    { uid : String
    , displayName : String
    , email : String
    }


type FireUser
    = FireUser Model


unwrap (FireUser model) =
    model


map fn =
    unwrap >> fn >> FireUser
