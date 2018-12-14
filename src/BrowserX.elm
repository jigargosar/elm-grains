module BrowserX exposing (WindowSize, focus)

import BasicsX exposing (unpackMaybe)
import Browser.Dom
import Task


type alias WindowSize =
    { width : Int, height : Int }


focus tagger domId =
    Browser.Dom.focus domId
        |> Task.mapError (\_ -> "FocusError: domId=" ++ domId)
        |> Task.attempt tagger
