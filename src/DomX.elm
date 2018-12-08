module DomX exposing (focus, focusMB)

import Browser.Dom as Dom
import Maybe as M
import Maybe.Extra as M
import Task


focus tag =
    Dom.focus >> Task.attempt (\_ -> tag)


focusMB tag maybeDomId =
    M.unwrap Cmd.none (focus tag) maybeDomId
