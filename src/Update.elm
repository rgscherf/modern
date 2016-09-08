module Update exposing (..)

import Math.Vector2 as V exposing (Vec2)
import Types exposing (..)
import Cmd.Extra as C


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Move s pt ->
            let
                newShip =
                    \s p -> { pos = V.add s.pos p }
            in
                ({ model | ship = newShip model.ship pt }) ! []

        KeyboardEvent char ->
            let
                speed =
                    10

                coords =
                    case char of
                        'd' ->
                            V.vec2 speed 0

                        's' ->
                            V.vec2 0 speed

                        'a' ->
                            V.vec2 (-speed) 0

                        'w' ->
                            V.vec2 0 (-speed)

                        _ ->
                            V.vec2 0 0
            in
                ( model, C.message (Move model.ship coords) )
