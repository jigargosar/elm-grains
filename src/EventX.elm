module EventX exposing
    ( Custom
    , CustomDecoder
    , KeyEvent
    , keyEventDecoder
    , onFocusIn
    , onFocusOut
    , onKeyDown
    , onKeyDownPD
    , pAll
    , pNone
    , pd
    , sp
    )

import Html.Events as HE
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


onKeyDown decoder =
    HE.on "keydown" decoder


onKeyDownPD =
    HE.preventDefaultOn "keydown"


onKeyDownCustom =
    HE.custom "keydown"


type alias CustomDecoder msg =
    Decoder (Custom msg)


type alias Custom msg =
    { message : msg, stopPropagation : Bool, preventDefault : Bool }


pAll msg =
    Custom msg True True


pNone msg =
    Custom msg False False


sp msg =
    Custom msg True False


pd msg =
    Custom msg False True


onFocusIn tagger =
    HE.on "focusin" (D.succeed tagger)


onFocusOut tagger =
    HE.on "focusout" (D.succeed tagger)


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
