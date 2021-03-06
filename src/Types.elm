module Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Random exposing (Seed)


type alias Model =
    { config : Config
    , playerShip : Entity
    , currentlyHighlightedTile : Vec2
    , entities : List Entity
    , enemySpawnCountdown : Int
    , entityId : Int
    , drawZapLine : Maybe ZapDirection
    }


type alias Config =
    { tileSize : Float
    , boardSize : Float
    , randomSeed : Seed
    , timeBetweenEnemySpawns : Int
    , chaseDistance : Float
    , numberOfLands : Int
    , playerMovementRange : Float
    }


type alias InitFlags =
    { startTime : Int }


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


type ZapDirection
    = ZapHorizontal
    | ZapVertical


type Msg
    = NoOp
    | Move Entity Vec2
    | CursorEnterTile Vec2
    | KeyboardEvent Char
    | SpawnEnemy
    | SpawnLands
    | MoveEnemies
    | Zap ZapDirection
