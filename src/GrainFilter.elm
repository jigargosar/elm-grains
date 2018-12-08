module GrainFilter exposing
    ( GrainFilter(..)
    , all
    , displayName
    , sList
    , toPred
    , viewModel
    )

import BasicsX exposing (allPass)
import Bucket exposing (Bucket)
import Grain
import SList


type GrainFilter
    = Todo
    | Completed
    | All
    | Trash
    | Bucket Bucket


sList =
    SList.fromList all


all =
    List.map Bucket Bucket.all
        ++ [ Todo
           , Completed
           , Trash
           , All
           ]


toPred filter =
    allPass <|
        case filter of
            Todo ->
                [ Grain.isPending, Grain.isInTrash >> not ]

            Completed ->
                [ Grain.isComplete, Grain.isInTrash >> not ]

            Bucket bucket ->
                [ Grain.isInBucket bucket, Grain.isInTrash >> not ]

            Trash ->
                [ Grain.isInTrash ]

            All ->
                [ always True, Grain.isInTrash >> not ]


viewModel filter =
    case filter of
        Todo ->
            { name = "Todo" }

        Completed ->
            { name = "Done" }

        Bucket bucket ->
            { name = Bucket.displayName bucket }

        Trash ->
            { name = "Trash" }

        All ->
            { name = "All" }


displayName =
    viewModel >> .name
