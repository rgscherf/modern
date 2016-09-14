module Main exposing (..)

import Math.Vector2 as V exposing (Vec2)
import Cmd.Extra as C
import Random exposing (Seed)
import SharedUtils exposing (..)
import AStar exposing (Position)
import Set exposing (Set)


--------
-- TYPES
--------


type alias Model =
    { playerShip : Ship
    , playerMovementRange : Float
    , tileSize : Float
    , boardSize : Float
    , currentlyHighlightedTile : Vec2
    , enemies : List Ship
    , enemySpawnCountdown : Int
    , randomSeed : Seed
    , timeBetweenEnemySpawns : Int
    }


type alias InitFlags =
    { startTime : Int }


init : InitFlags -> ( Model, Cmd Msg )
init { startTime } =
    { playerShip = Ship <| V.vec2 0 0
    , playerMovementRange = 2
    , tileSize = 30
    , boardSize = 24
    , currentlyHighlightedTile = V.vec2 0 0
    , enemies = []
    , enemySpawnCountdown = 3
    , randomSeed = Random.initialSeed startTime
    , timeBetweenEnemySpawns = 3
    }
        ! []


type alias Ship =
    { pos : Vec2 }


type TerrainType
    = Sea
    | Player
    | Enemy


type Msg
    = NoOp
    | Move Ship Vec2
    | ClickOnTile Vec2
    | CursorEnterTile Vec2
    | KeyboardEvent Char
    | SpawnEnemy
    | MoveEnemies



---------
-- UPDATE
---------


spawnNewEnemy : Model -> ( Ship, Seed )
spawnNewEnemy model =
    let
        allFreeCoords =
            makeMapCoords model.boardSize
                |> List.filter (tileIsNotBlocked model)

        generator =
            Random.int 0 ((List.length allFreeCoords) - 1)

        ( pickedIndex, newSeed ) =
            Random.step generator model.randomSeed

        newShipPos =
            Maybe.withDefault (V.vec2 0 0) (allFreeCoords !! pickedIndex)
    in
        ( { pos = newShipPos }, newSeed )


tileIsNotBlocked : Model -> Vec2 -> Bool
tileIsNotBlocked model vec =
    let
        entityPositions =
            model.playerShip.pos :: (List.map .pos model.enemies)
    in
        not <| List.member vec entityPositions


toPosition : Vec2 -> ( Int, Int )
toPosition v =
    V.toTuple v
        |> (\( x, y ) -> ( round x, round y ))


fromPosition : ( Int, Int ) -> Vec2
fromPosition ( x, y ) =
    V.vec2 (toFloat x) (toFloat y)


movesFrom : Model -> Position -> Set Position
movesFrom model p =
    let
        v =
            fromPosition p

        positions =
            [ V.vec2 (-1) 0
            , V.vec2 0 (-1)
            , V.vec2 0 1
            , V.vec2 1 0
            ]
    in
        List.map (\t -> V.add v t) positions
            |> List.filter (tileIsNotBlocked model)
            |> List.filter (tileIsInBounds model)
            |> List.map toPosition
            |> Set.fromList


tileIsInBounds : Model -> Vec2 -> Bool
tileIsInBounds model v =
    V.getX v > 0 && V.getX v < model.boardSize && V.getY v > 0 && V.getY v < model.boardSize


gridMoveCost : Position -> Position -> Float
gridMoveCost p p' =
    let
        xdist =
            abs <| (fst p) - (fst p')

        ydist =
            abs <| (snd p) - (snd p')
    in
        toFloat <| xdist + ydist


nextMove : Model -> Vec2 -> Vec2 -> Maybe (List Position)
nextMove model origin target =
    AStar.findPath
        gridMoveCost
        (movesFrom model)
        (toPosition origin)
        (toPosition target)


updateMoveSingleEnememy : Model -> Ship -> Ship
updateMoveSingleEnememy model ship =
    let
        positions =
            Debug.log "path to target..." <| nextMove model ship.pos model.playerShip.pos

        newPos =
            positions
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.withDefault (toPosition ship.pos)
                |> fromPosition
    in
        { pos = newPos }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        MoveEnemies ->
            { model
                | enemies =
                    List.map (updateMoveSingleEnememy model) model.enemies
            }
                ! []

        Move s pt ->
            let
                newX =
                    (V.getX <| V.add pt s.pos)

                newY =
                    (V.getY <| V.add pt s.pos)

                moveIsInBounds =
                    newX
                        >= 0
                        && newX
                        < model.boardSize
                        && newY
                        >= 0
                        && newY
                        < model.boardSize

                newVec =
                    V.vec2 newX newY
            in
                if moveIsInBounds && tileIsNotBlocked model newVec then
                    { model
                        | playerShip = { pos = newVec }
                        , enemySpawnCountdown = model.enemySpawnCountdown - 1
                    }
                        ! (if model.enemySpawnCountdown == 0 then
                            [ C.message SpawnEnemy, C.message MoveEnemies ]
                           else
                            [ C.message MoveEnemies ]
                          )
                else
                    model ! []

        SpawnEnemy ->
            let
                ( newShip, newSeed ) =
                    spawnNewEnemy model
            in
                { model
                    | enemySpawnCountdown = model.timeBetweenEnemySpawns
                    , enemies = newShip :: model.enemies
                    , randomSeed = newSeed
                }
                    ! []

        ClickOnTile vec ->
            model
                ! [ C.message <| Move model.playerShip vec ]

        CursorEnterTile vec ->
            { model | currentlyHighlightedTile = vec }
                ! []

        KeyboardEvent char ->
            let
                movePlayer v =
                    model ! [ C.message <| Move model.playerShip v ]
            in
                case char of
                    'w' ->
                        movePlayer <| V.vec2 0 (-1)

                    'a' ->
                        movePlayer <| V.vec2 (-1) 0

                    's' ->
                        movePlayer <| V.vec2 0 1

                    'd' ->
                        movePlayer <| V.vec2 1 0

                    'q' ->
                        movePlayer <| V.vec2 (-1) (-1)

                    'e' ->
                        movePlayer <| V.vec2 1 (-1)

                    'z' ->
                        movePlayer <| V.vec2 (-1) 1

                    'c' ->
                        movePlayer <| V.vec2 1 1

                    _ ->
                        model ! []
