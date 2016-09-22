module Utils.SharedUtils exposing (..)

import Math.Vector2 as V exposing (Vec2)
import Types exposing (..)


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


vequals : Vec2 -> Vec2 -> Bool
vequals v1 v2 =
    (V.getX v1 == V.getX v2) && (V.getY v1 == V.getY v2)


freeCoordsOnMap : Model -> List Vec2
freeCoordsOnMap model =
    -- list of free coordinates
    -- ( except for player's position )
    makeMapCoords model.config.boardSize
        |> List.filter (not << isTileOccupied model)
        |> List.filter (\a -> a /= model.playerShip.pos)


tileIsInBounds : Config -> Vec2 -> Bool
tileIsInBounds cfg v =
    (V.getX v > 0) && (V.getX v < cfg.boardSize) && (V.getY v > 0) && (V.getY v < cfg.boardSize)


isTileOccupied : Model -> Vec2 -> Bool
isTileOccupied model vec =
    model.entities
        |> List.map .pos
        |> List.member vec


makeMapCoords : Float -> List (Vec2)
makeMapCoords boardSize =
    List.concat <|
        List.map
            (\x -> List.map (\y -> V.vec2 x y) [0..boardSize])
            [0..boardSize]
