module BasicsX exposing
    ( Getter
    , Millis
    , Pred
    , PredList
    , add
    , allPass
    , applyMaybe
    , applyMaybeFn2
    , applyTo
    , applyTo2
    , atClampedIdx
    , call
    , callWith
    , callWith2
    , clampIdxIn
    , defaultEmptyStringTo
    , eq
    , eq0
    , eqBy
    , eqs
    , find
    , findIn
    , find_by
    , firstEq
    , flip
    , ifElse
    , isBlank
    , justWhen
    , listFromT2
    , listReject
    , listToggleMember
    , maybeBool
    , maybeWhen
    , neq
    , neqBy
    , notEq
    , notPred
    , partitionList2
    , propEq
    , replaceHead
    , rollIdx
    , safeModBy
    , swap
    , ter
    , unless
    , unlessBool
    , unpackMaybe
    , unpackResult
    , unwrapMaybe
    , when
    )

import List as L
import List.Extra as L


eqBy fn a1 a2 =
    fn a1 == fn a2


neqBy fn a1 a2 =
    eqBy fn a1 a2 |> not


listReject pred =
    List.filter (pred >> not)


add =
    (+)


notEq =
    (/=)


neq =
    notEq


eq =
    (==)


firstEq val =
    eqs val << Tuple.first


find : Pred a -> List a -> Maybe a
find pred =
    List.filter pred >> List.head


findIn : List a -> Pred a -> Maybe a
findIn =
    flip find


type alias Pred a =
    a -> Bool


type alias PredList a =
    List (Pred a)


allPass : PredList a -> Pred a
allPass plist a =
    List.all (applyTo a) plist


notPred : Pred a -> Pred a
notPred pred =
    pred >> not


type alias Getter big small =
    big -> small


propEq : Getter big small -> small -> Pred big
propEq getter small =
    getter >> eqs small


replaceHead newHead list =
    case list of
        head :: tail ->
            newHead :: tail

        [] ->
            []


type alias Millis =
    Int


ter b t f =
    if b then
        t

    else
        f


applyTo a fn =
    fn a


applyTo2 : a -> b -> (a -> b -> c) -> c
applyTo2 a b fn =
    fn a b


applyMaybeFn2 : a -> b -> Maybe (a -> b -> c) -> Maybe c
applyMaybeFn2 a b =
    Maybe.map (applyTo2 a b)


applyMaybe a =
    Maybe.map (applyTo a)


ifElse b t f v =
    ter (b v) (t v) (f v)


defaultEmptyStringTo : String -> String -> String
defaultEmptyStringTo =
    when String.isEmpty << always


when : (a -> Bool) -> (a -> a) -> a -> a
when b t =
    ifElse b t identity


unless b =
    when (b >> not)


unlessBool bool =
    when (always <| not bool)


eqs =
    (==)


eq0 =
    eqs 0


maybeBool bool value =
    if bool then
        Just value

    else
        Nothing


maybeWhen pred answerFn =
    ifElse pred (answerFn >> Just) (always Nothing)


justWhen pred =
    maybeWhen pred identity


unwrapMaybe : b -> (a -> b) -> Maybe a -> b
unwrapMaybe dv fn =
    Maybe.map fn >> Maybe.withDefault dv


unpackMaybe dvFn fn mb =
    case mb of
        Nothing ->
            dvFn

        Just val ->
            fn val


unpackResult errFn okFn result =
    case result of
        Ok answer ->
            okFn answer

        Err error ->
            errFn error


swap ( a, b ) =
    ( b, a )


isBlank =
    String.trim >> String.isEmpty


safeModBy total num =
    if total == 0 then
        0

    else
        modBy total num


listFromT2 : ( a, a ) -> List a
listFromT2 ( l1, l2 ) =
    [ l1, l2 ]


listToggleMember : a -> List a -> List a
listToggleMember item =
    ifElse (List.member item) (L.remove item) ((::) item)


partitionList2 : Pred a -> List a -> List (List a)
partitionList2 pred =
    List.partition pred >> listFromT2


compose : List (a -> a) -> a -> a
compose =
    List.foldl (<<) identity


flip f b a =
    f a b


call : (a -> b) -> a -> b
call fun =
    fun


callWith : a -> (a -> b) -> b
callWith a fn =
    fn a


callWith2 : a -> b -> (a -> b -> c) -> c
callWith2 a b fn =
    fn a b


find_by : (a -> b) -> b -> List a -> Maybe a
find_by insideDataFun data =
    List.filter (\e -> insideDataFun e == data)
        >> List.head


atClampedIdx list idx =
    if L.isEmpty list then
        Nothing

    else
        let
            clampedIdx =
                clamp 0 (L.length list - 1) idx
        in
        L.getAt clampedIdx list


clampIdxIn : List a -> Int -> Maybe Int
clampIdxIn list idx =
    if L.isEmpty list then
        Nothing

    else
        Just <| clamp 0 (L.length list - 1) idx


rollIdx : List a -> Int -> Maybe Int
rollIdx list idx =
    if L.isEmpty list then
        Nothing

    else
        Just <| safeModBy (List.length list) idx
