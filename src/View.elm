module View exposing (view)

import Types exposing (..)
import Math.Vector2 as V exposing (Vec2)
import Utils.SharedUtils exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as SE


queryMouseHighlight : Model -> Vec2 -> TerrainType
queryMouseHighlight model vec =
    if V.scale model.config.tileSize vec == model.playerShip.pos then
        Player
    else if List.member (V.scale model.config.tileSize vec) (List.map .pos model.entities) then
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

        Land ->
            MouseInfoDivPackage "Land ho!" "What might be buried here?"


view : Model -> Svg Msg
view model =
    let
        gameSizeStr =
            toString <| model.config.boardSize * model.config.tileSize

        mouseDivPackage =
            fillMouseInfoDiv <| queryMouseHighlight model model.currentlyHighlightedTile
    in
        Html.div []
            [ Html.div [ HA.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                [ Svg.svg
                    [ width gameSizeStr, height gameSizeStr ]
                  <|
                    (makeMapTiles model)
                        ++ (List.map (renderEntity model.config.tileSize) model.entities)
                        ++ (renderPlayer model)
                        ++ (renderLinedTiles model)
                , Html.div
                    [ HA.style [ ( "width", "400px" ), ( "background", "LightSlateGrey" ) ] ]
                    [ Html.h1 [] [ text <| mouseDivPackage.headline ]
                    , Html.h2 [] [ text <| mouseDivPackage.description ]
                    ]
                ]
            , Html.div [] [ text <| toString model ]
            ]


renderEntity : Float -> Entity -> Svg Msg
renderEntity tileSize entity =
    let
        pos' =
            V.toRecord entity.pos

        tileColor =
            case entity.entType of
                Ship ->
                    "Red"

                LandEntity ->
                    "Orange"
    in
        rect
            [ x <| toString <| pos'.x * tileSize
            , y <| toString <| pos'.y * tileSize
            , fill <| tileColor
            , width <| toString <| tileSize - 5
            , height <| toString <| tileSize - 5
            , SE.onMouseOver <| CursorEnterTile entity.pos
            ]
            []


makeMapTiles : Model -> List (Svg Msg)
makeMapTiles model =
    let
        boardCoords =
            makeMapCoords model.config.boardSize
    in
        List.map (makeOneSeaTile model) boardCoords


renderLinedTiles : Model -> List (Svg a)
renderLinedTiles model =
    let
        horizTiles =
            List.map (\x -> V.vec2 x (V.getY model.playerShip.pos)) [0..model.config.boardSize]

        vertTiles =
            List.map (\y -> V.vec2 (V.getX model.playerShip.pos) y) [0..model.config.boardSize]
    in
        case model.drawZapLine of
            Nothing ->
                []

            Just dir ->
                (case dir of
                    ZapHorizontal ->
                        horizTiles

                    ZapVertical ->
                        vertTiles
                )
                    |> List.filter (not << vequals model.playerShip.pos)
                    |> List.map (drawOneLine model)


drawOneLine : Model -> Vec2 -> Svg a
drawOneLine model pos =
    let
        screenPos =
            V.scale model.config.tileSize pos
                |> V.toRecord

        halfTile =
            model.config.tileSize / 2

        drawStart =
            case model.drawZapLine of
                Just ZapVertical ->
                    { screenPos | x = screenPos.x + halfTile, y = screenPos.y }

                Just ZapHorizontal ->
                    { screenPos | x = screenPos.x, y = screenPos.y + halfTile }

                Nothing ->
                    { x = 0, y = 0 }

        drawEnd =
            case model.drawZapLine of
                Just ZapVertical ->
                    { drawStart | x = drawStart.x, y = drawStart.y + model.config.tileSize }

                Just ZapHorizontal ->
                    { drawStart | x = drawStart.x + model.config.tileSize, y = drawStart.y }

                Nothing ->
                    { x = 0, y = 0 }
    in
        Svg.line
            [ stroke "blue"
            , x1 <| toString drawStart.x
            , y1 <| toString drawStart.y
            , x2 <| toString drawEnd.x
            , y2 <| toString drawEnd.y
            , strokeWidth <| toString 5
            ]
            []


makeOneSeaTile : Model -> Vec2 -> Svg Msg
makeOneSeaTile model pos =
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
            V.distance model.playerShip.pos pos <= (model.config.tileSize * model.config.playerMovementRange)

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
            [ x <| toString <| pos'.x * model.config.tileSize
            , y <| toString <| pos'.y * model.config.tileSize
            , fill <| tileColor
            , width <| toString model.config.tileSize
            , height <| toString model.config.tileSize
            , SE.onMouseOver <| CursorEnterTile pos
            ]
            []


renderPlayer : Model -> List (Svg Msg)
renderPlayer model =
    let
        playerPos' =
            V.toRecord model.playerShip.pos
    in
        [ rect
            [ x (toString <| playerPos'.x * model.config.tileSize)
            , y (toString <| playerPos'.y * model.config.tileSize)
            , fill "Aquamarine"
            , width <| toString <| model.config.tileSize - 5
            , height <| toString <| model.config.tileSize - 5
            , SE.onMouseOver <| CursorEnterTile model.playerShip.pos
            ]
            []
        ]
