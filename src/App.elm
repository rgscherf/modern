module App exposing (main)

import Html.App as App
import Keyboard
import View
import Types exposing (..)
import Main exposing (..)
import View exposing (..)
import Char


main : Program { startTime : Int }
main =
    App.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.presses (\k -> Char.fromCode k |> KeyboardEvent)
        ]
