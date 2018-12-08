module EventX exposing
    ( KeyEvent
    , keyEventDecoder
    , onFocusIn
    , onFocusInActiveElId
    , onFocusOut
    , onFocusOutActiveElId
    , onKeyDown
    , onKeyDownPD
    )

import Html.Events as HE
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


onKeyDown decoder =
    HE.on "keydown" decoder


onKeyDownPD =
    HE.preventDefaultOn "keydown"


onFocusIn tagger =
    HE.on "focusin" (D.succeed tagger)


onFocusOut tagger =
    HE.on "focusout" (D.succeed tagger)


activeElIdDecoder : Decoder String
activeElIdDecoder =
    D.at [ "target", "ownerDocument", "activeElement", "id" ] D.string


onFocusOutActiveElId tagger =
    HE.on "focusout" (D.map tagger activeElIdDecoder)


onFocusInActiveElId tagger =
    HE.on "focusin" (D.map tagger activeElIdDecoder)


type alias KeyEvent =
    { shiftKey : Bool
    , altKey : Bool
    , ctrlKey : Bool
    , metaKey : Bool
    , defaultPrevented : Bool
    , repeat : Bool
    , key : String
    , type_ : String
    , code : String
    }


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    D.succeed KeyEvent
        |> required "shiftKey" D.bool
        |> required "altKey" D.bool
        |> required "ctrlKey" D.bool
        |> required "metaKey" D.bool
        |> required "defaultPrevented" D.bool
        |> required "repeat" D.bool
        |> required "key" D.string
        |> required "type" D.string
        |> required "code" D.string
