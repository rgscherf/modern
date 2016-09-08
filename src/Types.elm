module Types exposing (..)

import Math.Vector2 exposing (..)


type alias Ship =
    { pos : Vec2 }


type alias Model =
    { ship : Ship }


type Msg
    = NoOp
    | Move Ship Vec2
    | KeyboardEvent Char
