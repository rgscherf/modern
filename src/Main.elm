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


type alias Config =
    { tileSize : Float
    , boardSize : Float
    , randomSeed : Seed
    , timeBetweenEnemySpawns : Int
    }


type alias Model =
    { config : Config
    , playerShip : Entity
    , playerMovementRange : Float
    , currentlyHighlightedTile : Vec2
    , entities : List Entity
    , enemySpawnCountdown : Int
    , entityId : Int
    }


type alias InitFlags =
    { startTime : Int }


init : InitFlags -> ( Model, Cmd Msg )
init { startTime } =
    { config = initConfig startTime
    , playerShip = Entity (V.vec2 0 0) Ship 0
    , playerMovementRange = 2
    , currentlyHighlightedTile = V.vec2 0 0
    , entities = []
    , enemySpawnCountdown = 3
    , entityId = 1
    }
        ! []


initConfig : Int -> Config
initConfig startTime =
    { tileSize = 30
    , boardSize = 24
    , randomSeed = Random.initialSeed startTime
    , timeBetweenEnemySpawns = 3
    }


type alias Entity =
    { pos : Vec2
    , entType : EntityType
    , entId : Int
    }


type EntityType
    = Ship
    | LandEntity


type TerrainType
    = Sea
    | Player
    | Enemy
    | Land


type Msg
    = NoOp
    | Move Entity Vec2
    | CursorEnterTile Vec2
    | KeyboardEvent Char
    | SpawnEnemy
    | MoveEnemies



---------
-- UPDATE
---------


spawnNewEnemy : Model -> ( Entity, Seed, Int )
spawnNewEnemy model =
    let
        allFreeCoords =
            makeMapCoords model.config.boardSize
                |> List.filter (not << isTileOccupied model Nothing)

        generator =
            Random.int 0 ((List.length allFreeCoords) - 1)

        ( pickedIndex, newSeed ) =
            Random.step generator model.config.randomSeed

        newShipPos =
            Maybe.withDefault (V.vec2 0 0) (allFreeCoords !! pickedIndex)
    in
        ( { pos = newShipPos, entType = Ship, entId = model.entityId }, newSeed, model.entityId + 1 )


isTileOccupied : Model -> Maybe Entity -> Vec2 -> Bool
isTileOccupied model mightBeMovingEntity vec =
    -- issue is that we block because the tile the object occupies,
    -- is occupied
    -- need to implement object IDs
    case mightBeMovingEntity of
        Nothing ->
            (model.playerShip :: model.entities)
                |> List.map .pos
                |> List.member vec

        Just ent ->
            List.filter (\a -> a.entId /= ent.entId) (model.playerShip :: model.entities)
                |> List.map .pos
                |> List.member vec


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
        positions' =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            ]
    in
        positions'
            |> List.map fromPosition
            |> List.filter (tileIsInBounds model.config)
            |> List.filter (not << isTileOccupied model (Just ent))
            |> List.map toPosition
            |> Set.fromList


tileIsInBounds : Config -> Vec2 -> Bool
tileIsInBounds cfg v =
    (V.getX v > 0) && (V.getX v < cfg.boardSize) && (V.getY v > 0) && (V.getY v < cfg.boardSize)


gridMoveCost : Position -> Position -> Float
gridMoveCost ( x, y ) ( x', y' ) =
    let
        xdist =
            abs <| (x - x')

        ydist =
            abs <| (y - y')
    in
        toFloat <| xdist + ydist


updateEnemies : Model -> List Entity -> List Entity -> List Entity
updateEnemies model updatedEntities notUpdateEntities =
    case notUpdateEntities of
        [] ->
            updatedEntities

        x :: xs ->
            let
                newModel =
                    { model | entities = updatedEntities ++ notUpdateEntities }
            in
                updateEnemies
                    newModel
                    (moveSingleEnemy newModel x :: updatedEntities)
                    xs


moveSingleEnemy : Model -> Entity -> Entity
moveSingleEnemy model entity =
    case entity.entType of
        LandEntity ->
            entity

        Ship ->
            let
                nm =
                    nextMove model entity entity.pos model.playerShip.pos

                newPos =
                    case nm of
                        Nothing ->
                            entity.pos

                        Just path ->
                            case List.head path of
                                Nothing ->
                                    entity.pos

                                Just p ->
                                    fromPosition p
            in
                { entity | pos = newPos }


nextMove : Model -> Entity -> Vec2 -> Vec2 -> Maybe (List Position)
nextMove model entity origin target =
    AStar.findPath
        gridMoveCost
        (movesFrom model entity)
        (toPosition origin)
        (toPosition target)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        MoveEnemies ->
            { model | entities = updateEnemies model [] model.entities }
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
                        < model.config.boardSize
                        && newY
                        >= 0
                        && newY
                        < model.config.boardSize

                newVec =
                    V.vec2 newX newY
            in
                if moveIsInBounds && (not <| isTileOccupied model Nothing newVec) then
                    { model
                      -- just update playership with new entity
                        | playerShip = Entity newVec Ship model.playerShip.entId
                        , enemySpawnCountdown = model.enemySpawnCountdown - 1
                    }
                        ! (if model.enemySpawnCountdown == 0 then
                            [ C.message MoveEnemies
                            , C.message SpawnEnemy
                            ]
                           else
                            [ C.message MoveEnemies ]
                          )
                else
                    model ! []

        SpawnEnemy ->
            let
                ( newShip, newSeed, newEntityId ) =
                    spawnNewEnemy model

                config =
                    model.config

                config' =
                    { config | randomSeed = newSeed }
            in
                { model
                    | enemySpawnCountdown = model.config.timeBetweenEnemySpawns
                    , entities = newShip :: model.entities
                    , config = config'
                    , entityId = newEntityId
                }
                    ! []

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
