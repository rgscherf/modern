module Main exposing (..)

import Html.App as App
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Math.Vector2 as V


main : Program Never
main =
    App.beginnerProgram { model = init, view = view, update = update }


init : Model
init =
    { ship = { pos = V.vec2 40 40 } }
