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
    , chaseDistance : Float
    , numberOfLands : Int
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
        ! [ C.message SpawnLands ]


makeLands : Model -> Int -> Model
makeLands model numSpawnsRemaining =
    case numSpawnsRemaining of
        0 ->
            model

        n ->
            let
                ( model', newLand ) =
                    spawnSingleLand model
            in
                makeLands model' (n - 1)


spawnSingleLand : Model -> ( Model, Entity )
spawnSingleLand model =
    let
        allfreecoords =
            freeCoordsOnMap model

        generator =
            Random.int 0 ((List.length allfreecoords) - 1)

        ( pickedindex, newseed ) =
            Random.step generator model.config.randomSeed

        newLand =
            { pos = Maybe.withDefault (V.vec2 0 0) <| allfreecoords !! pickedindex, entType = LandEntity, entId = model.entityId }

        cfg =
            model.config

        cfg' =
            { cfg | randomSeed = newseed }
    in
        ( { model | entities = newLand :: model.entities, config = cfg' }, newLand )


initConfig : Int -> Config
initConfig startTime =
    { tileSize = 30
    , boardSize = 24
    , randomSeed = Random.initialSeed startTime
    , timeBetweenEnemySpawns = 3
    , chaseDistance = 10
    , numberOfLands = round <| (24 * 24) / 8
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
    | SpawnLands
    | MoveEnemies



---------
-- UPDATE
---------


freeCoordsOnMap : Model -> List Vec2
freeCoordsOnMap model =
    -- list of free coordinates
    -- ( except for player's position )
    makeMapCoords model.config.boardSize
        |> List.filter (not << isTileOccupied model)
        |> List.filter (\a -> a /= model.playerShip.pos)


spawnNewEnemy : Model -> ( Entity, Seed, Int )
spawnNewEnemy model =
    let
        allfreecoords =
            freeCoordsOnMap model

        generator =
            Random.int 0 ((List.length allfreecoords) - 1)

        ( pickedIndex, newSeed ) =
            Random.step generator model.config.randomSeed

        newShipPos =
            Maybe.withDefault (V.vec2 0 0) (allfreecoords !! pickedIndex)
    in
        ( { pos = newShipPos
          , entType = Ship
          , entId = model.entityId
          }
        , newSeed
        , model.entityId + 1
        )


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


tileIsInBounds : Config -> Vec2 -> Bool
tileIsInBounds cfg v =
    (V.getX v > 0) && (V.getX v < cfg.boardSize) && (V.getY v > 0) && (V.getY v < cfg.boardSize)


isTileOccupied : Model -> Vec2 -> Bool
isTileOccupied model vec =
    model.entities
        |> List.map .pos
        |> List.member vec


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
                if moveIsInBounds && (not <| isTileOccupied model newVec) then
                    { model
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

        SpawnLands ->
            let
                model' =
                    makeLands model model.config.numberOfLands

                cfg =
                    model'.config

                cfg' =
                    { cfg | randomSeed = model'.config.randomSeed }
            in
                { model | config = cfg', entities = model'.entities } ! []

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
