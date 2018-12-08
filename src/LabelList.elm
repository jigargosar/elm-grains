module LabelList exposing
    ( LabelList
    , decodeString
    , getFromIdList
    , init
    , toList
    )

import Json.Decode as D
import Json.Encode as E
import Label exposing (Label)
import List.Extra as List
import Port
import UserLabel exposing (UserLabel)


type alias Model =
    List Label


type LabelList
    = LabelList Model


init : LabelList
init =
    fromList []


unwrap : LabelList -> Model
unwrap (LabelList model) =
    model


toList =
    unwrap


modify : (Model -> Model) -> LabelList -> LabelList
modify fn =
    unwrap >> fn >> LabelList


fromList : List Label -> LabelList
fromList =
    Label.addMissingSystemLabels >> LabelList


decodeString : String -> Result D.Error LabelList
decodeString =
    D.decodeString (D.list Label.decoder |> D.map fromList)


cacheCmd : LabelList -> Cmd msg
cacheCmd =
    unwrap >> E.list Label.encoder >> Port.cacheLabelList


getFromIdList idList =
    let
        pred label =
            List.member (Label.id label) idList
    in
    filter pred


filter pred =
    unwrap >> List.filter pred
