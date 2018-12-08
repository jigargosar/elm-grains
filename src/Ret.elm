module Ret exposing
    ( Return
    , andDo
    , andMapModel
    , andThen
    , andThenDo
    , andThenF
    , attemptMapModel
    , pure
    , unwrapMaybeCmd
    , withCmd
    , withEffect
    )

import BasicsX exposing (unwrapMaybe)
import Maybe as M
import Maybe.Extra as M
import Return
import Tuple exposing (mapSecond)


pure : m -> ( m, Cmd msg )
pure m =
    ( m, Cmd.none )


type alias Return msg model =
    Return.Return msg model


andThenDo =
    Return.effect_


withEffect fn =
    pure >> andThenDo fn


unwrapMaybeCmd f =
    M.unwrap Cmd.none f


andDo c2 ( m, c ) =
    ( m, Cmd.batch [ c, c2 ] )


withCmd c m =
    ( m, c )


andMapModel =
    Tuple.mapFirst


attemptMapModel fn ( model, cmd ) =
    fn model |> unwrapMaybe ( model, cmd ) (pure >> andDo cmd)


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )


andThenF f =
    andThen (\m -> pure m |> f m)
