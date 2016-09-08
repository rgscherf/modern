module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Model exposing (..)
import Math.Vector2 as V exposing (..)


getShipPos : Ship -> ( Float, Float )
getShipPos { pos } =
    ( V.getX pos, V.getY pos )


view : Model -> Svg Msg
view model =
    Html.div []
        [ Svg.svg [ width "400", height "400" ]
            [ circle [ cx "100", cy "100", r "25" ] []
            , rect
                [ x (toString <| fst <| getShipPos model.ship)
                , y (toString <| snd <| getShipPos model.ship)
                , fill "blue"
                , width "50"
                , height "50"
                ]
                []
            ]
        ]
