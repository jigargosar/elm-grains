module UserLabelList exposing (UserLabelList, decodeString, init)

import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Port
import SList exposing (SList)
import UserLabel exposing (UserLabel)


type alias Model =
    SList UserLabel


type UserLabelList
    = UserLabelList Model


init : UserLabelList
init =
    fromList []


unwrap : UserLabelList -> Model
unwrap (UserLabelList model) =
    model


modify : (Model -> Model) -> UserLabelList -> UserLabelList
modify fn =
    unwrap >> fn >> UserLabelList


fromList : List UserLabel -> UserLabelList
fromList =
    SList.fromList >> UserLabelList


decodeString : String -> Result D.Error UserLabelList
decodeString =
    D.decodeString (D.list UserLabel.decoder |> D.map fromList)


cacheCmd : UserLabelList -> Cmd msg
cacheCmd =
    unwrap >> SList.toList >> E.list UserLabel.encoder >> Port.cacheUserLabelList
