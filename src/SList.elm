module SList exposing
    ( SList
    , attempt
    , attemptSelectFirst
    , filter
    , filterAndRollBy
    , filterMap
    , filterMapCSToList
    , filteredSelected
    , fromList
    , map
    , mapCS
    , prepend
    , reject
    , rollBy
    , selectFilteredHead
    , selectHead
    , selected
    , toList
    , updateIf
    )

import BasicsX exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso as Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import SelectList exposing (SelectList)


type SList a
    = Empty
    | Selection (SelectList a)


fromList : List a -> SList a
fromList =
    SelectList.fromList >> unwrapMaybe Empty Selection


toList : SList a -> List a
toList sList =
    case sList of
        Empty ->
            []

        Selection selection ->
            SelectList.toList selection


selectionFromLR l r =
    case ( l, r ) of
        ( [], [] ) ->
            Empty

        ( f :: rest, [] ) ->
            let
                ( ll, cc ) =
                    List.unconsLast l
                        |> unwrapMaybe ( [], f )
                            (\( last, body ) -> ( body, last ))
            in
            fromLCR ll cc []

        ( _, c :: rest ) ->
            fromLCR l c rest


fromLCR : List a -> a -> List a -> SList a
fromLCR l c r =
    Selection <| SelectList.fromLists l c r


filter pred =
    mapLCR <|
        \( l, c, r ) ->
            let
                filterList =
                    List.filter pred

                ( ll, rr ) =
                    ( filterList l, filterList r )
            in
            if pred c |> not then
                selectionFromLR ll rr

            else
                fromLCR ll c rr


filterMap : (a -> Maybe b) -> SList a -> SList b
filterMap pred =
    mapLCR <|
        \( l, c, r ) ->
            let
                filterList =
                    List.filterMap pred

                ( ll, rr ) =
                    ( filterList l, filterList r )
            in
            Maybe.unpack (\_ -> selectionFromLR ll rr) (\cc -> fromLCR ll cc rr) (pred c)


filterAndRollBy : (a -> Bool) -> Int -> SList a -> SList a
filterAndRollBy pred offset sList =
    let
        selectItem item =
            attemptSelectFirst (eqs item) sList
    in
    filter pred sList
        |> rollBy offset
        |> selected
        |> Maybe.unwrap sList selectItem


reject pred =
    filter (pred >> not)


list2BodyLastT2 : List a -> Maybe ( List a, a )
list2BodyLastT2 =
    List.unconsLast >> Maybe.map swap


singleton c =
    fromLCR [] c []


rollBy : Int -> SList a -> SList a
rollBy offset =
    overSelection (SelectList.selectWhileLoopBy offset)


mapCS : (a -> b) -> (a -> b) -> SList a -> SList b
mapCS cFn sFn =
    mapLCR (\( l, c, r ) -> fromLCR (List.map sFn l) (cFn c) (List.map sFn r))


filterMapCSToList : (a -> Bool) -> (a -> b) -> (a -> b) -> SList a -> List b
filterMapCSToList pred cFn sFn =
    filter pred
        >> mapCS cFn sFn
        >> toList


mapLCR : (( List a, a, List a ) -> SList b) -> SList a -> SList b
mapLCR fn sList =
    case sList of
        Empty ->
            Empty

        Selection s ->
            fn (SelectList.toTuple s)


map fn =
    mapCS fn fn


updateIf pred fn =
    map (when pred fn)


selected sList =
    case sList of
        Empty ->
            Nothing

        Selection s ->
            Just <| SelectList.selected s


filteredSelected pred =
    filter pred >> selected


overSelectionWithDefault def fn sList =
    case sList of
        Empty ->
            def

        Selection s ->
            Selection <| fn s


overSelection : (SelectList a -> SelectList a) -> SList a -> SList a
overSelection =
    overSelectionWithDefault Empty


attempt : (SelectList a -> Maybe (SelectList a)) -> SList a -> SList a
attempt fn =
    overSelection (SelectList.attempt fn)


prepend : a -> SList a -> SList a
prepend item =
    overSelectionWithDefault (singleton item)
        (SelectList.selectHead >> SelectList.insertAfter item)


attemptSelectFirst pred =
    let
        selectBefore =
            SelectList.selectBeforeIf pred

        selectAfter =
            SelectList.selectAfterIf pred
    in
    attempt
        (\s ->
            selectBefore s
                |> Maybe.orElseLazy (\_ -> selectAfter s)
        )


selectHead =
    overSelection SelectList.selectHead


selectFilteredHead pred sList =
    let
        selectItem item =
            attemptSelectFirst (eqs item) sList
    in
    filter pred sList
        |> selected
        |> Maybe.unwrap sList selectItem
