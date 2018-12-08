module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (Style, alignItems, alignSelf, backgroundColor, center, color, column, displayFlex, flexDirection, flexGrow, fontSize, hex, int, justifyContent, maxWidth, minHeight, minWidth, num, overflow, padding, pct, px, rem, rgb, rgba, vh, vw, zero)
import CssElements exposing (..)
import CssIcons exposing (viewIcon)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import Cursor exposing (Cursor)
import DecodeX exposing (DecodeResult)
import DomX
import Either exposing (Either(..))
import Elevation exposing (elevation)
import EventX
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainList exposing (GrainList)
import HotKey as K exposing (SoftKey(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Material.Icons.Action as MIcons
import Material.Icons.Alert as MIcons
import Material.Icons.Content as MIcons
import Material.Icons.Editor as MIcons
import Material.Icons.Navigation as MIcons
import Material.Icons.Toggle as MIcons
import Maybe.Extra as Maybe
import Msg exposing (GM(..), Msg(..))
import Palette exposing (black, black10, white)
import Port
import QueryPrefix
import Random exposing (Generator, Seed)
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import SList exposing (SList)
import Task
import Theme exposing (spacingUnit)
import TimeX exposing (Millis)
import Tuple exposing (mapFirst)



---- MODEL ----


type alias Flags =
    { now : Millis
    , windowSize : WindowSize
    , grains : String
    }


type alias Model =
    { grains : GrainList
    }


init : Flags -> Return Msg Model
init flags =
    let
        grainsReturn : Return msg GrainList
        grainsReturn =
            GrainList.decodeString flags.grains
                |> DecodeX.resultToReturn GrainList.init
    in
    Return.map
        (\grains ->
            { grains = grains
            }
        )
        grainsReturn


setGrains grains model =
    { model | grains = grains }


overGrainList : (GrainList -> GrainList) -> Model -> Model
overGrainList fn model =
    setGrains (fn model.grains) model


addGrain : Grain -> Model -> Model
addGrain grain =
    overGrainList (GrainList.prepend grain)


removeGrain : GrainId -> Model -> Model
removeGrain gid =
    overGrainList (GrainList.remove gid)


overGrainWithId : GrainId -> (Grain -> Grain) -> Model -> Model
overGrainWithId gid fn =
    overGrainList (GrainList.update gid fn)


overSelectedGrain fn model =
    model


moveSelectedGrainToBucket bucket =
    overSelectedGrain (Grain.moveToBucket bucket)



---- UPDATE ----


cacheGrainListEffect : Model -> Cmd msg
cacheGrainListEffect =
    .grains >> GrainList.cacheCmd


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (globalKeyBinding model) ]


andThenUpdate msg =
    Return.andThen (update msg)


updateDispatcher msg model =
    let
        config =
            HandlerConfig
                { handler = handle
                , model = model
                , cmd = Cmd.none
                }

        toReturn =
            unwrapConfig >> (\c -> ( c.model, c.cmd ))
    in
    handle msg config
        |> toReturn


handlerFromConfig =
    unwrapConfig >> .handler


modelFromConfig =
    unwrapConfig >> .model


unwrapConfig (HandlerConfig hc) =
    hc


overConfig fn =
    unwrapConfig >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model -> HandlerConfig msg model
dispatch msg config =
    handlerFromConfig config |> callWith2 msg config


modModel fn =
    overConfig (\c -> { c | model = fn c.model })


andDo cmd =
    overConfig (\c -> { c | cmd = Cmd.batch [ c.cmd, cmd ] })


andThenDo fn c =
    andDo (fn (modelFromConfig c)) c


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        config =
            HandlerConfig
                { handler = updateF
                , model = model
                , cmd = Cmd.none
                }

        toReturn =
            unwrapConfig >> (\c -> ( c.model, c.cmd ))
    in
    handlerFromConfig config msg config |> toReturn


toElmUpdateFn handler msg model =
    let
        config =
            HandlerConfig
                { handler = updateF
                , model = model
                , cmd = Cmd.none
                }

        toReturn =
            unwrapConfig >> (\c -> ( c.model, c.cmd ))
    in
    handlerFromConfig config msg config |> toReturn


updateF message =
    case message of
        NoOp ->
            identity

        LogError errMsg ->
            andDo (Port.error errMsg)

        SubGM msg ->
            case msg of
                GMNew title ->
                    andDo (Grain.newGeneratorWithTitleCmd (SubGM << GMOnGen) title)

                GMOnGen gen ->
                    andDo (Random.generate (SubGM << GMAdd) gen)

                GMAdd grain ->
                    modModel (addGrain grain)
                        >> andThenDo cacheGrainListEffect

        Prev ->
            identity

        Next ->
            identity


handle : Msg -> HandlerConfig Msg Model -> HandlerConfig Msg Model
handle msg =
    case msg of
        NoOp ->
            identity

        _ ->
            dispatch NoOp


type HandlerConfig msg model
    = HandlerConfig
        { handler : msg -> HandlerConfig msg model -> HandlerConfig msg model
        , model : model
        , cmd : Cmd msg
        }


keyBinding model =
    K.bindEachToMsg <|
        [ ( K.arrowUp, ( NoOp, True ) )
        , ( K.arrowDown, ( NoOp, True ) )
        ]


view : Model -> Html Msg
view model =
    styled div
        [ Css.flexShrink <| num 0, Css.minWidth <| pct 100, Css.minHeight <| pct 100 ]
        [ id "base-layer"
        , class "sans-serif flex flex-column"
        , tabindex -1
        , SA.fromUnstyled <|
            EventX.onKeyDownPD <|
                keyBinding model
        ]
        [ viewBase model
        ]


viewBase : Model -> Html Msg
viewBase model =
    styled div
        []
        [ class "flex flex-column items-center" ]
        [ styled div
            [ Css.width <| px 400 ]
            [ class "flex flex-column pv3" ]
            [ viewGrains model.grains
            ]
        ]


viewGrains : GrainList -> Html Msg
viewGrains grainList =
    let
        viewItem selected grain =
            viewGrainItem
                { selected = selected
                , gid = Grain.id grain
                , domId = GrainList.grainDomId grain
                , title = Grain.title grain
                }
                grain
    in
    div [ id "grains-container", class "flex flex-column pv2" ]
        (grainList |> GrainList.toList |> List.map (viewItem False))


viewGrainItem { domId, gid, selected, title } grain =
    let
        viewGrainTitle =
            flexRow []
                [ class "f4 pa1" ]
                [ text <|
                    if isBlank title then
                        "<no title>"

                    else
                        title
                ]
    in
    flexCol []
        [ id domId
        , class "pa2 bb b--light-gray"
        , classList [ ( "bg-lightest-blue", selected ) ]
        ]
        [ flexRow [ Css.justifyContent Css.spaceBetween ]
            [ class "" ]
            [ viewGrainTitle
            ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = toElmUpdateFn updateF

        --        , update = updateDispatcher
        , subscriptions = subscriptions
        }
