module Grain exposing (Grain, mockList, title)


type alias Model =
    { title : String
    }


type Grain
    = Grain Model


unwrap (Grain model) =
    model


map fn =
    unwrap >> fn >> Grain


title =
    unwrap >> .title


fromTitle =
    Model >> Grain


mockList =
    List.map fromTitle [ "a", "b" ]
