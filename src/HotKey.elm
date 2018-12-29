module HotKey exposing
    ( Event
    , HotKey
    , SoftKey(..)
    , arrowDown
    , arrowLeft
    , arrowRight
    , arrowUp
    , bindEach
    , bindEachToMsg
    , char
    , ctrlDown
    , ctrlLeft
    , ctrlRight
    , ctrlUp
    , decoder
    , delete
    , enter
    , esc
    , isArrowKey
    , isHotKey
    , metaArrow
    , metaDown
    , metaLeft
    , metaRight
    , metaUp
    , shiftEnter
    , shiftMetaEnter
    , space
    )

import BasicsX exposing (..)
import Direction exposing (Direction)
import EventX exposing (KeyEvent)
import Html
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List as L
import List.Extra as L
import Maybe as M
import Maybe.Extra as M
import Tuple exposing (first, second)


type SoftKey
    = Shift
    | Alt
    | Ctrl
    | Meta


type alias Event =
    ( List SoftKey, String )


type alias HotKey =
    ( List SoftKey, String )


appendWhen bool value list =
    if bool then
        list ++ [ value ]

    else
        list


fromKeyEvent : KeyEvent -> HotKey
fromKeyEvent { shiftKey, altKey, ctrlKey, metaKey, key } =
    ( []
        |> appendWhen shiftKey Shift
        |> appendWhen altKey Alt
        |> appendWhen ctrlKey Ctrl
        |> appendWhen metaKey Meta
    , key
    )


matchesKeyEvent : KeyEvent -> HotKey -> Bool
matchesKeyEvent ke =
    eq (fromKeyEvent ke)


isHotKey : HotKey -> KeyEvent -> Bool
isHotKey =
    flip matchesKeyEvent


isArrowKey ke =
    [ arrowUp, arrowDown, arrowLeft, arrowRight ]
        |> List.any (matchesKeyEvent ke)


decoder : Decoder HotKey
decoder =
    D.map fromKeyEvent EventX.keyEventDecoder


bindEachToMsg : List ( HotKey, msg ) -> Decoder msg
bindEachToMsg mappings =
    decoder
        |> D.andThen
            (firstEq
                >> findIn mappings
                >> unwrapMaybe
                    (D.fail "No Handler found")
                    (Tuple.second >> D.succeed)
            )


bindEach : List ( HotKey, KeyEvent -> msg ) -> Decoder msg
bindEach mappings =
    EventX.keyEventDecoder
        |> D.andThen
            (\keyEvent ->
                let
                    maybeHandler =
                        mappings |> L.find (first >> matchesKeyEvent keyEvent) |> M.map second
                in
                maybeHandler
                    |> M.map (callWith keyEvent)
                    |> unwrapMaybe (D.fail "No Handler found") D.succeed
            )


enter : HotKey
enter =
    ( [], "Enter" )


shiftEnter : HotKey
shiftEnter =
    ( [ Shift ], "Enter" )


shiftMetaEnter : HotKey
shiftMetaEnter =
    ( [ Shift, Meta ], "Enter" )


metaArrow : Direction -> HotKey
metaArrow direction =
    let
        dir2String =
            case direction of
                Direction.Up ->
                    "ArrowUp"

                Direction.Down ->
                    "ArrowDown"

                Direction.Left ->
                    "ArrowLeft"

                Direction.Right ->
                    "ArrowRight"
    in
    ( [ Meta ], dir2String )


metaUp : HotKey
metaUp =
    ( [ Meta ], "ArrowUp" )


metaDown : HotKey
metaDown =
    ( [ Meta ], "ArrowDown" )


metaLeft : HotKey
metaLeft =
    ( [ Meta ], "ArrowLeft" )


metaRight : HotKey
metaRight =
    ( [ Meta ], "ArrowRight" )


ctrlDown : HotKey
ctrlDown =
    ( [ Ctrl ], "ArrowDown" )


ctrlUp : HotKey
ctrlUp =
    ( [ Ctrl ], "ArrowUp" )


ctrlLeft : HotKey
ctrlLeft =
    ( [ Ctrl ], "ArrowLeft" )


ctrlRight : HotKey
ctrlRight =
    ( [ Ctrl ], "ArrowRight" )


delete : HotKey
delete =
    ( [], "Delete" )


esc : HotKey
esc =
    ( [], "Escape" )


arrowDown : HotKey
arrowDown =
    ( [], "ArrowDown" )


arrowUp : HotKey
arrowUp =
    ( [], "ArrowUp" )


arrowLeft : HotKey
arrowLeft =
    ( [], "ArrowLeft" )


arrowRight : HotKey
arrowRight =
    ( [], "ArrowRight" )


space : HotKey
space =
    ( [], " " )


char : Char -> HotKey
char c =
    ( [], String.fromChar c )
