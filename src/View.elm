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
