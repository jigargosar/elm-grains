module TodoLI exposing
    ( FuzzyTodo
    , Item(..)
    , Msg(..)
    , displayTitle
    , doneCheckBox
    , getSelectionIndicatorDomId
    , initList
    , view
    , xSelectionIndicator
    )

import BasicsX exposing (..)
import El exposing (..)
import Element as E exposing (Attribute, Element, el, focused, mouseOver)
import Element.Border as Border
import Element.Events as EE exposing (onClick)
import Element.Input as EI
import EventX
import Fuzzy
import HotKey as HK
import Html.Attributes exposing (class, id)
import Icons
import List as L
import List.Extra as L
import MaterialColorsUI exposing (..)
import Maybe as M
import Maybe.Extra as M
import Todo exposing (Todo, TodoStore)


type alias FuzzyValue a =
    { value : a, result : Fuzzy.Result }


initFuzzyValue : a -> Fuzzy.Result -> FuzzyValue a
initFuzzyValue a result =
    { value = a, result = result }


getScore : FuzzyValue a -> Int
getScore =
    .result >> .score


isScoreGreaterThan num { result } =
    result.score > num


isScoreLessThan num { result } =
    result.score < num


type alias FuzzyTodo =
    FuzzyValue Todo


type Item
    = FuzzyTodoLI FuzzyTodo
    | CreateTodoLI String



--    | InputField String


toFuzzyTodoList : String -> List Todo -> List FuzzyTodo
toFuzzyTodoList query =
    let
        boil =
            String.toLower

        filterFn todo =
            Fuzzy.match [] [] (boil query) (boil <| todo.title)
                |> initFuzzyValue todo
    in
    L.map filterFn


initList : { query : String, todoStore : TodoStore } -> List Item
initList { query, todoStore } =
    let
        fuzzyTodoList =
            Todo.all todoStore
                |> toFuzzyTodoList query

        items =
            if isBlank query then
                fuzzyTodoList |> L.map FuzzyTodoLI

            else
                fuzzyTodoList
                    |> L.filter (isScoreLessThan 1000)
                    |> L.sortBy getScore
                    |> L.map FuzzyTodoLI
                    |> (::) (CreateTodoLI query)
    in
    items


itemDomIdPrefix =
    "todo-li--"


getItemDomId item =
    itemDomIdPrefix
        ++ (case item of
                FuzzyTodoLI { value } ->
                    value.id

                CreateTodoLI title ->
                    "create-todo-action"
           )


getSelectionIndicatorDomId item =
    let
        itemDomId =
            getItemDomId item
    in
    itemDomId ++ "--" ++ xSelectionIndicator


type Msg
    = Update Todo Todo.Msg
    | Create
    | PD


view : { selected : Bool, item : Item } -> Element Msg
view { selected, item } =
    let
        rootEl : List (Attribute Msg) -> List (Element Msg) -> Element Msg
        rootEl attrs =
            r <|
                ([ fHA <| id <| getItemDomId item
                 , s1
                 , fw
                 , fh
                 , bwb 1
                 , bc <| blackA 0.1
                 ]
                    ++ attrs
                )

        selectionIndicator =
            el
                [ fHA <| id <| getSelectionIndicatorDomId item
                , ti_1
                , bwr 3
                , fh
                , bc <|
                    if selected then
                        blue400

                    else
                        a0
                , onKeyDownPD <|
                    HK.bindEachToMsg
                        [ ( HK.arrowDown, ( PD, True ) )
                        , ( HK.arrowUp, ( PD, True ) )
                        ]
                ]
                (t "")

        rootView =
            case item of
                FuzzyTodoLI fuzzyTodo ->
                    let
                        todo =
                            fuzzyTodo.value
                    in
                    rootEl []
                        [ selectionIndicator
                        , r [ fw ]
                            [ doneCheckBox todo
                            , displayTitle todo.title
                            ]
                        ]

                CreateTodoLI title ->
                    rootEl [ onClick Create ]
                        [ selectionIndicator
                        , displayTitle " + add task"
                        ]
    in
    rootView


xSelectionIndicator =
    "x-selection-indicator"


displayTitle title =
    el [ fw, p3, ti_1 ] (t title)


doneCheckBox todo =
    EI.checkbox
        [ p1
        , ti_1
        , sw
        , brPill
        , fc grey500
        , focused [ Border.glow blue200 3, fc grey800 ]
        , mouseOver [ Border.glow blueGrey300 1, fc grey800 ]
        , onKeyDownPD <|
            HK.bindEachToMsg
                [ ( HK.space, ( PD, True ) )
                ]
        ]
        { label = lh "done"
        , icon =
            \checked ->
                r [ fw, fh ]
                    [ ter checked Icons.checkCircleOutline Icons.circleOutline
                    ]
        , checked = todo.done
        , onChange = Update todo << Todo.SetDone
        }
