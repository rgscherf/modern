module Utils.Pathfinding exposing (moveSingleEnemy)

import Types exposing (..)
import AStar exposing (Position)
import Math.Vector2 as V exposing (Vec2)
import Set exposing (Set)
import Utils.SharedUtils exposing (..)


gridMoveCost : Position -> Position -> Float
gridMoveCost ( x, y ) ( x', y' ) =
    let
        xdist =
            abs <| (x - x')

        ydist =
            abs <| (y - y')
    in
        toFloat <| xdist + ydist


getPathTo : Model -> Entity -> Vec2 -> Maybe (List Position)
getPathTo model entity target =
    AStar.findPath
        gridMoveCost
        (movesFrom model entity)
        (toPosition <| entity.pos)
        (toPosition target)


moveSingleEnemy : Model -> Entity -> Entity
moveSingleEnemy model entity =
    case entity.entType of
        LandEntity ->
            entity

        Ship ->
            let
                isInChasingDistance =
                    V.distance entity.pos model.playerShip.pos < model.config.chaseDistance

                pathTo =
                    if isInChasingDistance then
                        getPathTo model entity model.playerShip.pos
                    else
                        Nothing

                newPos =
                    case pathTo of
                        Nothing ->
                            entity.pos

                        Just path ->
                            case List.head path of
                                Nothing ->
                                    entity.pos

                                Just p ->
                                    if fromPosition p == model.playerShip.pos then
                                        entity.pos
                                    else
                                        fromPosition p
            in
                { entity | pos = newPos }


toPosition : Vec2 -> ( Int, Int )
toPosition v =
    V.toTuple v
        |> (\( x, y ) -> ( round x, round y ))


fromPosition : ( Int, Int ) -> Vec2
fromPosition ( x, y ) =
    V.vec2 (toFloat x) (toFloat y)


movesFrom : Model -> Entity -> Position -> Set Position
movesFrom model ent ( x, y ) =
    let
        possibleMoves =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            ]
    in
        possibleMoves
            |> List.map fromPosition
            |> List.filter (tileIsInBounds model.config)
            |> List.filter (not << isTileOccupied model)
            |> List.map toPosition
            |> Set.fromList
