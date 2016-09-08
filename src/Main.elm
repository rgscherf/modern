module Main exposing (..)

import Html.App as App
import Types exposing (..)
import Update exposing (..)
import View exposing (..)
import Math.Vector2 as V
import Keyboard
import Char


main : Program Never
main =
    App.program { init = ( init, Cmd.none ), update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.presses (\k -> Char.fromCode k |> KeyboardEvent)
        ]


init : Model
init =
    { ship = { pos = V.vec2 40 40 } }
