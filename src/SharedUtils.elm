module SharedUtils exposing (..)

import Math.Vector2 as V exposing (Vec2)


(!!) : List a -> Int -> Maybe a
(!!) l ind =
    let
        go l' ind' =
            if ind' <= 0 then
                List.head l'
            else
                go (Maybe.withDefault [] <| List.tail l') (ind' - 1)
    in
        go l ind
infixr 1 !!


makeMapCoords : Float -> List (Vec2)
makeMapCoords boardSize =
    List.concat <|
        List.map
            (\x -> List.map (\y -> V.vec2 x y) [0..boardSize])
            [0..boardSize]
