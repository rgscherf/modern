module View exposing (view)

import Main exposing (..)
import Math.Vector2 as V exposing (Vec2)
import SharedUtils exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as SE


queryMouseHighlight : Model -> Vec2 -> TerrainType
queryMouseHighlight model vec =
    if V.scale model.config.tileSize vec == model.playerShip.pos then
        Player
    else if List.member (V.scale model.config.tileSize vec) (List.map .pos model.enemies) then
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
                        ++ (List.map (renderEnemy model.config.tileSize) model.enemies)
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
            [ x <| toString <| pos'.x * tileSize
            , y <| toString <| pos'.y * tileSize
            , fill <| "Red"
            , width <| toString tileSize
            , height <| toString tileSize
            , SE.onMouseOver <| CursorEnterTile pos
            ]
            []


makeMapTiles : Model -> List (Svg Msg)
makeMapTiles model =
    let
        boardCoords =
            makeMapCoords model.config.boardSize
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
            V.distance model.playerShip.pos pos <= (model.config.tileSize * model.playerMovementRange)

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


getShipPos : Ship -> ( Float, Float )
getShipPos { pos } =
    ( V.getX pos, V.getY pos )


renderPlayer : Model -> List (Svg Msg)
renderPlayer model =
    [ rect
        [ x (toString <| (\a -> a * model.config.tileSize) <| fst <| getShipPos model.playerShip)
        , y (toString <| (\a -> a * model.config.tileSize) <| snd <| getShipPos model.playerShip)
        , fill "Aquamarine"
        , width <| toString model.config.tileSize
        , height <| toString model.config.tileSize
        , SE.onMouseOver <| CursorEnterTile model.playerShip.pos
        ]
        []
    ]
