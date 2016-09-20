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
    , playerShip = Entity (V.vec2 0 0) Ship
    , playerMovementRange = 2
    , currentlyHighlightedTile = V.vec2 0 0
    , entities = []
    , enemySpawnCountdown = 3
    }
        ! []


initConfig : Int -> Config
initConfig startTime =
    { tileSize = 30
    , boardSize = 24
    , randomSeed = Random.initialSeed startTime
    , timeBetweenEnemySpawns = 3
    }



-- type alias Ship =
--     { pos : Vec2 }
-- type alias Island =
--     { pos : Vec2 }


type alias Entity =
    { pos : Vec2
    , entType : EntityType
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


spawnNewEnemy : Model -> ( Entity, Seed )
spawnNewEnemy model =
    let
        allFreeCoords =
            makeMapCoords model.config.boardSize
                |> List.filter (not << isTileBlocked model)

        generator =
            Random.int 0 ((List.length allFreeCoords) - 1)

        ( pickedIndex, newSeed ) =
            Random.step generator model.config.randomSeed

        newShipPos =
            Maybe.withDefault (V.vec2 0 0) (allFreeCoords !! pickedIndex)
    in
        ( { pos = newShipPos, entType = Ship }, newSeed )


allObjectPositions : Model -> List Vec2
allObjectPositions model =
    List.map .pos <| model.playerShip :: model.entities


isTileBlocked : Model -> Vec2 -> Bool
isTileBlocked model vec =
    -- issue is that we block because the tile the object occupies,
    -- is occupied
    -- need to implement object IDs
    allObjectPositions model
        |> List.member vec


toPosition : Vec2 -> ( Int, Int )
toPosition v =
    V.toTuple v
        |> (\( x, y ) -> ( round x, round y ))


fromPosition : ( Int, Int ) -> Vec2
fromPosition ( x, y ) =
    V.vec2 (toFloat x) (toFloat y)


movesFrom : Model -> Position -> Set Position
movesFrom model ( x, y ) =
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
            |> List.filter (not << isTileBlocked model)
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
updateEnemies model updatedEnemies notUpdatedEnemies =
    case notUpdatedEnemies of
        [] ->
            updatedEnemies

        x :: xs ->
            let
                newModel =
                    { model | enemies = updatedEnemies ++ notUpdatedEnemies }
            in
                updateEnemies
                    newModel
                    (moveSingleEnemy newModel x :: updatedEnemies)
                    xs


moveSingleEnemy : Model -> Entity -> Entity
moveSingleEnemy model ship =
    let
        nm =
            nextMove model ship.pos model.playerShip.pos

        newPos =
            case nm of
                Nothing ->
                    ship.pos

                Just path ->
                    case List.head path of
                        Nothing ->
                            ship.pos

                        Just p ->
                            fromPosition p
    in
        { pos = newPos }


nextMove : Model -> Vec2 -> Vec2 -> Maybe (List Position)
nextMove model origin target =
    AStar.findPath
        gridMoveCost
        (movesFrom model)
        (toPosition origin)
        (toPosition target)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        MoveEnemies ->
            { model | enemies = updateEnemies model [] model.enemies }
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
                if moveIsInBounds && (not <| isTileBlocked model newVec) then
                    { model
                        | playerShip = { pos = newVec }
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
                ( newShip, newSeed ) =
                    spawnNewEnemy model

                config =
                    model.config

                config' =
                    { config | randomSeed = newSeed }
            in
                { model
                    | enemySpawnCountdown = model.config.timeBetweenEnemySpawns
                    , enemies = newShip :: model.enemies
                    , config = config'
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
