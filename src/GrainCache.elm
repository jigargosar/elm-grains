module GrainCache exposing
    ( GrainCache
    , addNewGrain
    , addNewGrainAfter
    , addNewGrainBefore
    , batchUpdate
    , childGrains
    , decoder
    , empty
    , encoder
    , firstRootGid
    , get
    , isDescendent
    , lastLeafGid
    , load
    , moveBy
    , moveOneLevelDown
    , moveOneLevelUp
    , nextGid
    , parentGidOfGid
    , prevGid
    , remove
    , rootGrains
    , setSaved
    , toList
    , updateFromFirebaseChangeList
    , updateWithGrainUpdate
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, callWith2, eqs, ifElse, unwrapMaybe)
import Compare
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import GrainIdLookup exposing (GrainIdLookup)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomX
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)
import SavedGrain exposing (SavedGrain)
import Time exposing (Posix)


type alias GrainCache =
    GrainIdLookup SavedGrain


decoder : Decoder GrainCache
decoder =
    D.list SavedGrain.decoder
        |> D.map (GrainIdLookup.fromList SavedGrain.id)


encoder : GrainCache -> Value
encoder =
    GrainIdLookup.toList >> E.list SavedGrain.encoder


empty : GrainCache
empty =
    GrainIdLookup.empty


getGrainById : GrainId -> GrainCache -> Maybe Grain
getGrainById gid =
    get gid >> Maybe.map SavedGrain.value



-- EXPOSED GID NAV HELPERS --


firstRootGid : GrainCache -> Maybe GrainId
firstRootGid =
    firstRoot >> Maybe.map Grain.id


lastLeafGid : GrainCache -> Maybe GrainId
lastLeafGid =
    lastLeaf >> Maybe.map Grain.id


nextGid : GrainId -> GrainCache -> Maybe GrainId
nextGid gid model =
    getGrainById gid model
        |> Maybe.map (nextGrainOrSame >> callWith model >> Grain.id)


prevGid : GrainId -> GrainCache -> Maybe GrainId
prevGid gid model =
    getGrainById gid model
        |> Maybe.map (prevGrainOrSame >> callWith model >> Grain.id)



-- ABSOLUTE GRAIN HELPERS --


lastLeaf : GrainCache -> Maybe Grain
lastLeaf model =
    lastRoot model |> Maybe.map (lastLeafOf >> callWith model)


firstRoot : GrainCache -> Maybe Grain
firstRoot =
    rootGrains >> List.head


lastRoot : GrainCache -> Maybe Grain
lastRoot =
    rootGrains >> List.last



-- RELATIVE GRAIN HELPERS --


firstChildOf : Grain -> GrainCache -> Maybe Grain
firstChildOf grain =
    childGrains grain >> List.head


lastChildOf : Grain -> GrainCache -> Maybe Grain
lastChildOf grain =
    childGrains grain >> List.last


nextGrainOrSame : Grain -> GrainCache -> Grain
nextGrainOrSame grain model =
    firstChildOf grain model
        |> Maybe.orElseLazy
            (\_ ->
                nextSiblingOf grain model
            )
        |> Maybe.orElseLazy
            (\_ ->
                nextSiblingOfParentOf grain model
            )
        |> Maybe.withDefault grain


prevGrainOrSame : Grain -> GrainCache -> Grain
prevGrainOrSame grain model =
    prevSiblingOf grain model
        |> Maybe.map (lastLeafOf >> callWith model)
        |> Maybe.orElseLazy
            (\_ -> parentGrain grain model)
        |> Maybe.withDefault grain


prevSiblingOf : Grain -> GrainCache -> Maybe Grain
prevSiblingOf grain model =
    siblingsOf grain model
        |> List.takeWhile (Grain.eqById grain >> not)
        |> List.last


nextSiblingOf : Grain -> GrainCache -> Maybe Grain
nextSiblingOf grain model =
    siblingsOf grain model
        |> List.dropWhile (Grain.eqById grain >> not)
        |> List.drop 1
        |> List.head


nextSiblingOfParentOf : Grain -> GrainCache -> Maybe Grain
nextSiblingOfParentOf grain model =
    let
        rec : Grain -> Maybe Grain
        rec parent =
            nextSiblingOf parent model
                |> Maybe.orElseLazy (\_ -> nextSiblingOfParentOf parent model)
    in
    parentGrain grain model |> Maybe.andThen rec


rootGrains =
    allGrains >> List.filter Grain.isRoot


allGrains : GrainCache -> List Grain
allGrains =
    toList
        >> List.map SavedGrain.value
        >> List.sortWith Grain.defaultComparator


childGrains : Grain -> GrainCache -> List Grain
childGrains grain =
    allGrains >> List.filter (Grain.isChildOf grain)


siblingsOf : Grain -> GrainCache -> List Grain
siblingsOf grain =
    allGrains >> List.filter (Grain.isSibling grain)


parentGrain : Grain -> GrainCache -> Maybe Grain
parentGrain grain model =
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (getGrainById >> callWith model)


lastLeafOf : Grain -> GrainCache -> Grain
lastLeafOf grain model =
    let
        children =
            childGrains grain model
    in
    if List.isEmpty children then
        grain

    else
        List.last children
            |> Maybe.unwrap grain (lastLeafOf >> callWith model)



--- OLD CODE ---


nextSiblingGidOfGid : GrainId -> GrainCache -> Maybe GrainId
nextSiblingGidOfGid gid model =
    getSiblingsById gid model
        |> List.dropWhile (idEq gid >> not)
        |> List.drop 1
        |> List.head
        |> Maybe.map id


getSiblingsById : GrainId -> GrainCache -> List SavedGrain
getSiblingsById gid model =
    get gid model |> Maybe.unwrap [] (getSiblingsOf__ >> callWith model)


get : GrainId -> GrainCache -> Maybe SavedGrain
get gid =
    GrainIdLookup.get gid


getParentOfGrain : Grain -> GrainCache -> Maybe SavedGrain
getParentOfGrain grain model =
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (GrainIdLookup.get >> callWith model)


getParentOf : SavedGrain -> GrainCache -> Maybe SavedGrain
getParentOf savedGrain model =
    parentIdAsGrainId savedGrain
        |> Maybe.andThen (GrainIdLookup.get >> callWith model)


parentGidOfGid : GrainId -> GrainCache -> Maybe GrainId
parentGidOfGid gid =
    get gid >> Maybe.andThen parentIdAsGrainId


getParentIdOfGid : GrainId -> GrainCache -> Maybe Grain.ParentId
getParentIdOfGid gid =
    get gid >> Maybe.map parentId


isDescendent : Grain -> Grain -> GrainCache -> Bool
isDescendent descendent ancestor model =
    if Grain.isParentOf descendent ancestor then
        True

    else
        getParentOfGrain descendent model
            |> Maybe.unwrap False
                (SavedGrain.value
                    >> isDescendent
                    >> callWith2 ancestor model
                )


getAncestorIdsOfGrain grain model =
    getAncestorIdsHelp [] grain model


getAncestorIdsHelp ids grain model =
    let
        gid =
            Grain.id grain

        newIds =
            gid :: ids
    in
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (get >> callWith model >> Maybe.map SavedGrain.value)
        |> Maybe.unwrap newIds (getAncestorIdsHelp newIds >> callWith model)


toList =
    GrainIdLookup.toList


setSaved : Grain -> GrainCache -> GrainCache
setSaved grain =
    GrainIdLookup.update (Grain.id grain)
        (Maybe.map (SavedGrain.setSaved grain)
            >> Maybe.orElseLazy (\_ -> Just <| SavedGrain.new grain)
        )


remove grain =
    GrainIdLookup.remove (Grain.id grain)


type alias UpdateResult =
    Result String GrainCache


addNewGrain : Grain -> GrainCache -> UpdateResult
addNewGrain =
    ifCanAddGrainThen <|
        \grain model ->
            Result.Ok <|
                insertBlind grain model


addNewGrainAfter : GrainId -> Grain -> GrainCache -> UpdateResult
addNewGrainAfter siblingGid =
    ifCanAddGrainThen <|
        \grain model ->
            let
                now =
                    Grain.createdAt grain

                maybeNewParentId =
                    getParentIdOfGid siblingGid model

                siblings =
                    getSiblingsById siblingGid model

                newIdx =
                    List.findIndex (idEq siblingGid) siblings
                        |> Maybe.map ((+) 1)

                insertGrainBetween ( left, right ) =
                    left ++ [ grain ] ++ right

                maybeSortIndexUpdaters =
                    newIdx
                        |> Maybe.map
                            (List.splitAt
                                >> callWith (List.map SavedGrain.value siblings)
                                >> insertGrainBetween
                                >> Grain.listToEffectiveSortIndices
                                >> List.map
                                    (Tuple.mapBoth
                                        (Grain.SetSortIdx >> Grain.update now)
                                        Grain.id
                                    )
                            )
            in
            Maybe.map2
                (\pid updaters ->
                    let
                        gid =
                            Grain.id grain
                    in
                    insertBlind grain model
                        |> updateWithGrainUpdate (Grain.SetParentId pid) gid now
                        |> Result.andThen (batchUpdate updaters)
                )
                maybeNewParentId
                maybeSortIndexUpdaters
                |> Maybe.withDefault (Result.Err "Err: addNewGrainAfter")


addNewGrainBefore : GrainId -> Grain -> GrainCache -> UpdateResult
addNewGrainBefore siblingGid =
    ifCanAddGrainThen <|
        \grain model ->
            let
                now =
                    Grain.createdAt grain

                maybeNewParentId =
                    getParentIdOfGid siblingGid model

                siblings =
                    getSiblingsById siblingGid model

                newIdx =
                    List.findIndex (idEq siblingGid) siblings
                        |> Maybe.map ((+) 0)

                insertGrainBetween ( left, right ) =
                    left ++ [ grain ] ++ right

                maybeSortIndexUpdaters =
                    newIdx
                        |> Maybe.map
                            (List.splitAt
                                >> callWith (List.map SavedGrain.value siblings)
                                >> insertGrainBetween
                                >> Grain.listToEffectiveSortIndices
                                >> List.map
                                    (Tuple.mapBoth
                                        (Grain.SetSortIdx >> Grain.update now)
                                        Grain.id
                                    )
                            )
            in
            Maybe.map2
                (\pid updaters ->
                    let
                        gid =
                            Grain.id grain
                    in
                    insertBlind grain model
                        |> updateWithGrainUpdate (Grain.SetParentId pid) gid now
                        |> Result.andThen (batchUpdate updaters)
                )
                maybeNewParentId
                maybeSortIndexUpdaters
                |> Maybe.withDefault (Result.Err "Err: addNewGrainAfter")


ifCanAddGrainThen :
    (Grain -> GrainCache -> UpdateResult)
    -> Grain
    -> GrainCache
    -> UpdateResult
ifCanAddGrainThen fn grain model =
    if idExists (Grain.id grain) model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        fn grain model


idExists gid =
    GrainIdLookup.member gid


insertBlind grain model =
    GrainIdLookup.insert (Grain.id grain) (SavedGrain.new grain) model


update :
    (Grain -> Grain)
    -> GrainId
    -> GrainCache
    -> UpdateResult
update changeFn gid model =
    if GrainIdLookup.member gid model then
        let
            updateFn =
                SavedGrain.change changeFn
        in
        Result.Ok <| GrainIdLookup.updateIfExists gid updateFn model

    else
        Result.Err "GrainNotFound"


batchUpdate : List ( Grain -> Grain, GrainId ) -> GrainCache -> UpdateResult
batchUpdate list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (update changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


updateWithGrainUpdate :
    Grain.Update
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
updateWithGrainUpdate grainUpdate gid now model =
    update (Grain.update now grainUpdate) gid model


moveBy :
    Int
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
moveBy offset gid now model =
    get gid model
        |> Result.fromMaybe "Error: setSortIdx: Grain Not Found in Cache"
        |> Result.andThen (moveHelp now offset >> callWith model)


moveOneLevelUp gid now model =
    getGrainById gid model
        |> Maybe.andThen (moveOneLevelUpHelp now model)
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelDown gid now model =
    let
        siblings =
            getSiblingsById gid model

        newParentIdx : Int
        newParentIdx =
            List.findIndex (idEq gid) siblings
                |> Maybe.unwrap -1 ((+) -1)
                |> Debug.log "newParentIdx"
    in
    List.getAt newParentIdx siblings
        |> Maybe.map
            (SavedGrain.value
                >> Grain.idAsParentId
                >> Debug.log "pid"
                >> (\pid ->
                        updateWithGrainUpdate
                            (Grain.SetParentId pid)
                            gid
                            now
                            model
                   )
            )
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelUpHelp now model grain =
    getParentOfGrain grain model
        |> Maybe.map (SavedGrain.value >> moveGrainAfter now model grain)


moveGrainAfter now model grain sibling =
    let
        gid =
            Grain.id grain

        newParentId =
            Grain.parentId sibling
    in
    updateWithGrainUpdate (Grain.SetParentId newParentId) gid now model


moveHelp : Posix -> Int -> SavedGrain -> GrainCache -> UpdateResult
moveHelp now offset savedGrain model =
    let
        siblings : List SavedGrain
        siblings =
            getSiblingsOf__ savedGrain model

        gIdx : Int
        gIdx =
            List.findIndex (eqById savedGrain)
                siblings
                |> Maybe.withDefault -1

        updaters : List ( Grain -> Grain, GrainId )
        updaters =
            List.swapAt gIdx (gIdx + offset) siblings
                |> List.map SavedGrain.value
                |> Grain.listToEffectiveSortIndices
                |> List.map
                    (Tuple.mapBoth
                        (Grain.SetSortIdx >> Grain.update now)
                        Grain.id
                    )
    in
    batchUpdate updaters model


getSiblingsOf__ : SavedGrain -> GrainCache -> List SavedGrain
getSiblingsOf__ savedGrain model =
    toList model
        |> List.filter (eqByParentId savedGrain)
        |> List.sortWith defaultComparator


getChildrenWithParentId pid model =
    toList model
        |> List.filter (eqByParentId pid)
        |> List.sortWith defaultComparator


defaultComparator =
    Compare.compose SavedGrain.value Grain.defaultComparator


eqByParentId savedGrain =
    SavedGrain.value
        >> Grain.isSibling (SavedGrain.value savedGrain)


idEq gid =
    SavedGrain.value >> Grain.idEq gid


eqById savedGrain =
    SavedGrain.value
        >> Grain.eqById (SavedGrain.value savedGrain)


updateFromFirebaseChangeList changeList model =
    let
        handleChange change =
            let
                grain =
                    GrainChange.grain change
            in
            case GrainChange.type_ change of
                GrainChange.Added ->
                    setSaved grain

                GrainChange.Modified ->
                    setSaved grain

                GrainChange.Removed ->
                    remove grain
    in
    List.foldr handleChange model changeList
        |> Result.Ok


load =
    D.decodeValue decoder
        >> Result.mapError D.errorToString


id =
    SavedGrain.id


isRoot =
    SavedGrain.value >> Grain.isRoot


parentId =
    SavedGrain.value >> Grain.parentId


isChildOfGrainId gid =
    SavedGrain.value >> Grain.isChildOfGrainId gid


parentIdAsGrainId =
    SavedGrain.value >> Grain.parentIdAsGrainId
