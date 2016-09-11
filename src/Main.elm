module Main exposing (..)

import Html.App as App
import Math.Vector2 as V exposing (Vec2)
import Cmd.Extra as C
import Html
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as SE
import Keyboard
import Char


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
    }


init : Model
init =
    { playerShip = Ship <| V.vec2 0 0
    , playerMovementRange = 2
    , tileSize = 30
    , boardSize = 24
    , currentlyHighlightedTile = V.vec2 0 0
    , enemies = []
    , enemySpawnCountdown = 3
    }


type alias Ship =
    { pos : Vec2 }


type Msg
    = NoOp
    | Move Ship Vec2
    | ClickOnTile Vec2
    | CursorEnterTile Vec2
    | KeyboardEvent Char



---------
-- UPDATE
---------


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
            in
                if moveIsInBounds then
                    { model
                        | playerShip = { pos = V.vec2 newX newY }
                        , enemySpawnCountdown = model.enemySpawnCountdown - 1
                    }
                        ! (if model.enemySpawnCountdown == 0 then
                            []
                           else
                            []
                          )
                else
                    model ! []

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


view : Model -> Svg Msg
view model =
    let
        gameSizeStr =
            toString <| model.boardSize * model.tileSize
    in
        Html.div []
            [ Html.div [ HA.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                [ Svg.svg
                    [ width gameSizeStr, height gameSizeStr ]
                  <|
                    List.append (makeMapTiles model) (renderPlayer model)
                , Html.div
                    [ HA.style [ ( "width", "400px" ), ( "background", "LightSlateGrey" ) ] ]
                    [ text "Hello world" ]
                ]
            , Html.div [] [ text <| toString model ]
            ]


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
            , SE.onClick <| ClickOnTile pos
            , SE.onMouseOver <| CursorEnterTile pos
            ]
            []


getShipPos : Ship -> ( Float, Float )
getShipPos { pos } =
    ( V.getX pos, V.getY pos )


renderPlayer : Model -> List (Svg a)
renderPlayer model =
    [ rect
        [ x (toString <| fst <| getShipPos model.playerShip)
        , y (toString <| snd <| getShipPos model.playerShip)
        , fill "Aquamarine"
        , width <| toString model.tileSize
        , height <| toString model.tileSize
        ]
        []
    ]



-------
-- MAIN
-------


main : Program Never
main =
    App.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.presses (\k -> Char.fromCode k |> KeyboardEvent)
        ]
