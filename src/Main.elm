module Main exposing (..)

import Html.App as App
import Math.Vector2 as V exposing (Vec2)
import Cmd.Extra as C
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as SE
import Keyboard
import Char
import Random exposing (Seed)


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



---------
-- UPDATE
---------


(!!) : List a -> Int -> Maybe a
(!!) l ind =
    let
        go l' ind' =
            if ind' <= 0 then
                List.head l'
            else
                go (Maybe.withDefault [] (List.tail l')) (ind' - 1)
    in
        go l ind
infixr 1 !!


spawnNewEnemy : Model -> ( Ship, Seed )
spawnNewEnemy model =
    let
        allCoords =
            makeMapCoords model.boardSize model.tileSize

        allFreeCoords =
            List.filter (tileIsNotBlocked model) allCoords

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
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
                        < (model.tileSize * model.boardSize)
                        && newY
                        >= 0
                        && newY
                        < (model.tileSize * model.boardSize)

                newVec =
                    V.vec2 newX newY
            in
                if moveIsInBounds && tileIsNotBlocked model newVec then
                    { model
                        | playerShip = { pos = newVec }
                        , enemySpawnCountdown = model.enemySpawnCountdown - 1
                    }
                        ! (if model.enemySpawnCountdown == 0 then
                            [ C.message SpawnEnemy ]
                           else
                            []
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
                    model ! [ C.message <| Move model.playerShip <| V.scale model.tileSize v ]
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



-------
-- VIEW
-------


queryMouseHighlight : Model -> Vec2 -> TerrainType
queryMouseHighlight model vec =
    if vec == model.playerShip.pos then
        Player
    else if List.member vec (List.map .pos model.enemies) then
        Enemy
    else
        Sea


type alias MouseInfoDivPackage =
    { headline : String
    , description : String
    }


fillMouseInfoDiv : TerrainType -> MouseInfoDivPackage
fillMouseInfoDiv terr =
    case terr of
        Player ->
            MouseInfoDivPackage "It's you!" "The intrepid trader"

        Enemy ->
            MouseInfoDivPackage "A pirate" "Run or fight"

        Sea ->
            MouseInfoDivPackage "Open sea" "Nothing to see..."


view : Model -> Svg Msg
view model =
    let
        gameSizeStr =
            toString <| model.boardSize * model.tileSize

        mouseDivPackage =
            fillMouseInfoDiv <| queryMouseHighlight model model.currentlyHighlightedTile
    in
        Html.div []
            [ Html.div [ HA.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                [ Svg.svg
                    [ width gameSizeStr, height gameSizeStr ]
                  <|
                    (makeMapTiles model)
                        ++ (List.map (renderEnemy model.tileSize) model.enemies)
                        ++ (renderPlayer model)
                , Html.div
                    [ HA.style [ ( "width", "400px" ), ( "background", "LightSlateGrey" ) ] ]
                    [ Html.h1 [] [ text <| mouseDivPackage.headline ]
                    , Html.h2 [] [ text <| mouseDivPackage.description ]
                    ]
                ]
            , Html.div [] [ text <| toString model ]
            ]


renderEnemy : Float -> Ship -> Svg Msg
renderEnemy tileSize { pos } =
    let
        pos' =
            V.toRecord pos
    in
        rect
            [ x <| toString pos'.x
            , y <| toString pos'.y
            , fill <| "Red"
            , width <| toString tileSize
            , height <| toString tileSize
            , SE.onMouseOver <| CursorEnterTile pos
            ]
            []


makeMapCoords : Float -> Float -> List (Vec2)
makeMapCoords boardSize tileSize =
    List.concat <|
        List.map
            (\x -> List.map (\y -> V.vec2 (x * tileSize) (y * tileSize)) [0..boardSize])
            [0..boardSize]


makeMapTiles : Model -> List (Svg Msg)
makeMapTiles model =
    let
        boardCoords =
            makeMapCoords model.boardSize model.tileSize
    in
        List.map (makeOneRect model) boardCoords


makeOneRect : Model -> Vec2 -> Svg Msg
makeOneRect model pos =
    let
        pos' =
            V.toRecord pos

        notHighlightedColor =
            "RoyalBlue"

        notHighligtedButInRange =
            -- 10% lighter than royal blue
            "#4f74e3"

        highlightedInRangeColor =
            "Salmon"

        highligtedOutOfRangeColor =
            -- darker than royal blue
            "#2251dd"

        isInRange =
            V.distance model.playerShip.pos pos <= (model.tileSize * model.playerMovementRange)

        isHighligtedTile =
            model.currentlyHighlightedTile == pos

        tileColor =
            if isHighligtedTile then
                if isInRange then
                    highlightedInRangeColor
                else
                    highligtedOutOfRangeColor
            else if isInRange then
                notHighligtedButInRange
            else
                notHighlightedColor
    in
        rect
            [ x <| toString pos'.x
            , y <| toString pos'.y
            , fill <| tileColor
            , width <| toString model.tileSize
            , height <| toString model.tileSize
            , SE.onMouseOver <| CursorEnterTile pos
            ]
            []


getShipPos : Ship -> ( Float, Float )
getShipPos { pos } =
    ( V.getX pos, V.getY pos )


renderPlayer : Model -> List (Svg Msg)
renderPlayer model =
    [ rect
        [ x (toString <| fst <| getShipPos model.playerShip)
        , y (toString <| snd <| getShipPos model.playerShip)
        , fill "Aquamarine"
        , width <| toString model.tileSize
        , height <| toString model.tileSize
        , SE.onMouseOver <| CursorEnterTile model.playerShip.pos
        ]
        []
    ]



-------
-- MAIN
-------


main : Program { startTime : Int }
main =
    App.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.presses (\k -> Char.fromCode k |> KeyboardEvent)
        ]
