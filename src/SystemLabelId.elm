module SystemLabelId exposing
    ( SystemLabelId(..)
    , decoder
    , displayName
    , encoder
    , getMissing
    )

import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Set


type SystemLabelId
    = Trash
    | Todo


encoder label =
    E.string <|
        case label of
            Trash ->
                "TRASH"

            Todo ->
                "INCOMPLETE"


decoder =
    D.string
        |> D.andThen
            (\strId ->
                case strId of
                    "TRASH" ->
                        Trash |> D.succeed

                    "INCOMPLETE" ->
                        Todo |> D.succeed

                    _ ->
                        D.fail "Invalid label id"
            )


filterDefaultList pred =
    List.filter pred [ Todo, Trash ]


getMissing : List SystemLabelId -> List SystemLabelId
getMissing idList =
    filterDefaultList (\id -> List.notMember id idList)


displayName id =
    case id of
        Trash ->
            "Trash"

        Todo ->
            "Todo"
