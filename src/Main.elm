module Main exposing (..)

import Types exposing (..)
import Math.Vector2 as V exposing (Vec2)
import Cmd.Extra as C
import Random exposing (Seed)
import Utils.SharedUtils exposing (..)
import Utils.Pathfinding exposing (..)
import Utils.MakeIslands exposing (..)


--------
-- INITS
--------


init : InitFlags -> ( Model, Cmd Msg )
init { startTime } =
    { config = initConfig startTime
    , playerShip = Entity (V.vec2 0 0) Ship 0
    , currentlyHighlightedTile = V.vec2 0 0
    , entities = []
    , enemySpawnCountdown = 3
    , entityId = 1
    , drawZapLine = Nothing
    }
        ! [ C.message SpawnLands ]


initConfig : Int -> Config
initConfig startTime =
    { tileSize = 30
    , boardSize = 24
    , randomSeed = Random.initialSeed startTime
    , timeBetweenEnemySpawns = 3
    , chaseDistance = 10
    , numberOfLands = round <| (24 * 24) / 8
    , playerMovementRange = 2
    }



---------
-- UPDATE
---------


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


isZapped : Model -> ZapDirection -> Entity -> Bool
isZapped model dir ent =
    case ent.entType of
        Ship ->
            case dir of
                ZapHorizontal ->
                    V.getY ent.pos == V.getY model.playerShip.pos

                ZapVertical ->
                    V.getX ent.pos == V.getX model.playerShip.pos

        LandEntity ->
            False


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
                        , drawZapLine = Nothing
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

        Zap dir ->
            let
                filterzapped =
                    List.filter (not << isZapped model dir) model.entities
            in
                { model
                    | entities = filterzapped
                    , drawZapLine = Just dir
                }
                    ! []

        KeyboardEvent char ->
            let
                movePlayer v =
                    model ! [ C.message <| Move model.playerShip v ]

                zapFromPlayer d =
                    model ! [ C.message <| Zap d ]
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

                    'i' ->
                        zapFromPlayer ZapVertical

                    'k' ->
                        zapFromPlayer ZapVertical

                    'j' ->
                        zapFromPlayer ZapHorizontal

                    'l' ->
                        zapFromPlayer ZapHorizontal

                    _ ->
                        model ! []
