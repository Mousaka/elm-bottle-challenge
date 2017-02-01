module App exposing (..)

import State exposing (updateWithCmd, init)
import Types exposing (Msg, Model)
import Html exposing (program)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program { init = init ! [], view = view, subscriptions = (\_ -> Sub.none), update = updateWithCmd }
