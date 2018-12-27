module GrainZipper exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , appendWhenIdEqAndGetParentAndChildGrains
    , backwardFromRootWhenIdEq
    , forwardFromRootWhenIdEq
    , fromTree
    , lastDescendentGrain
    , parentWhenIdEq
    , prependWhenIdEqAndGetParentAndChildGrains
    , removeEqByIdThenFlatten
    )

import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Maybe.Extra as Maybe
import Tree exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)


type alias Model =
    Zipper Grain


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree Grain


type GrainZipper
    = GrainZipper Model


unwrap (GrainZipper model) =
    model


map fn =
    unwrap >> fn >> GrainZipper


mapMaybe fn =
    unwrap >> fn >> Maybe.map GrainZipper


fromTree : GrainTree -> GrainZipper
fromTree =
    Z.fromTree >> GrainZipper


findFromRootEqById : Grain -> GrainZipper -> Maybe GrainZipper
findFromRootEqById grain =
    Z.findFromRoot (Grain.eqById grain)
        |> mapMaybe


findFromRootIdEq : GrainId -> GrainZipper -> Maybe GrainZipper
findFromRootIdEq gid =
    Z.findFromRoot (Grain.idEq gid)
        |> mapMaybe


removeTree : GrainZipper -> Maybe GrainZipper
removeTree =
    Z.removeTree
        |> mapMaybe


removeEqById : Grain -> GrainZipper -> Maybe GrainZipper
removeEqById grain =
    findFromRootEqById grain
        >> Maybe.andThen removeTree


removeEqByIdThenFlatten : Grain -> GrainZipper -> List Grain
removeEqByIdThenFlatten grain =
    removeEqById grain >> Maybe.unwrap [] flatten


flatten : GrainZipper -> List Grain
flatten =
    unwrap >> Z.toTree >> Tree.flatten


lastDescendantZ =
    Z.lastDescendant
        |> map


label =
    unwrap >> Z.label


lastDescendentGrain =
    lastDescendantZ >> label


appendGrain : Grain -> GrainZipper -> GrainZipper
appendGrain grain =
    Z.append (Tree.singleton grain)
        |> map


appendWhenIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
appendWhenIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.map (appendGrain grain)


prependWhenIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
prependWhenIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.map (appendGrain grain)


appendWhenIdEqAndGetParentAndChildGrains :
    GrainId
    -> Grain
    -> GrainZipper
    -> Maybe ( Grain, List Grain )
appendWhenIdEqAndGetParentAndChildGrains gid grain =
    appendWhenIdEq gid grain
        >> Maybe.andThen toParentChildrenLabelsTuple


prependWhenIdEqAndGetParentAndChildGrains :
    GrainId
    -> Grain
    -> GrainZipper
    -> Maybe ( Grain, List Grain )
prependWhenIdEqAndGetParentAndChildGrains gid grain =
    appendWhenIdEq gid grain
        >> Maybe.andThen toParentChildrenLabelsTuple


toParentChildrenLabelsTuple =
    unwrap
        >> (\z ->
                let
                    children =
                        Z.children z |> List.map Tree.label
                in
                Z.parent z
                    |> Maybe.map (Z.label >> (\p -> ( p, children )))
           )


forward =
    Z.forward
        |> mapMaybe


backward =
    Z.backward
        |> mapMaybe


parent =
    Z.parent
        |> mapMaybe


forwardFromRootWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen forward
        >> Maybe.map label


backwardFromRootWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen backward
        >> Maybe.map label


parentWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen parent
        >> Maybe.map label
