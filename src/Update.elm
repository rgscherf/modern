module Update exposing (..)

import Math.Vector2 as V exposing (Vec2)
import Model exposing (..)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Move s pt ->
            let
                newShip =
                    \s p -> { pos = V.add s.pos p }
            in
                { model | ship = newShip model.ship pt }
