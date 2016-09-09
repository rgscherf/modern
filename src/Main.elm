module Main exposing (..)

import Html.App as App
import Math.Vector2 as V exposing (Vec2)
import Cmd.Extra as C
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as SE


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
        [-- Keyboard.presses (\k -> Char.fromCode k |> KeyboardEvent)
        ]


init : Model
init =
    { ship = { pos = V.vec2 0 0 } }


type alias Ship =
    { pos : Vec2 }


type alias Model =
    { ship : Ship }


type Msg
    = NoOp
    | Move Ship Vec2
    | ClickOnTile Vec2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Move s pt ->
            ({ model | ship = { pos = pt } }) ! []

        ClickOnTile vec ->
            ( model, C.message <| Move model.ship vec )


getShipPos : Ship -> ( Float, Float )
getShipPos { pos } =
    ( V.getX pos, V.getY pos )


makeMapCoords : Float -> List ( Float, Float )
makeMapCoords boardSize =
    List.concat <|
        List.map
            (\x -> List.map (\y -> ( x, y )) [0..boardSize])
            [0..boardSize]


makeMapTiles : Float -> Float -> List (Svg Msg)
makeMapTiles boardSize tileSize =
    let
        boardCoords =
            makeMapCoords boardSize
    in
        List.map (makeOneRect tileSize) boardCoords


makeOneRect : Float -> ( Float, Float ) -> Svg Msg
makeOneRect tileSize ( x', y' ) =
    let
        backgroundColor =
            "AliceBlue"

        x'' =
            x' * tileSize

        y'' =
            y' * tileSize
    in
        rect
            [ x <| toString x''
            , y <| toString y''
            , fill backgroundColor
            , width <| toString tileSize
            , height <| toString tileSize
            , stroke "LavenderBlush"
            , strokeWidth "1px"
            , SE.onClick <| ClickOnTile (V.vec2 x'' y'')
            , Se.onHover <| 
            ]
            []


view : Model -> Svg Msg
view model =
    let
        boardSize =
            10

        tileSize =
            50

        gameSizeStr =
            toString <| boardSize * tileSize
    in
        Html.div []
            [ Svg.svg [ width gameSizeStr, height gameSizeStr ] <|
                (makeMapTiles boardSize tileSize)
                    ++ [ rect
                            [ x (toString <| fst <| getShipPos model.ship)
                            , y (toString <| snd <| getShipPos model.ship)
                            , fill "LightPink"
                            , width <| toString tileSize
                            , height <| toString tileSize
                            ]
                            []
                       ]
            ]
