module Utils.MakeIslands exposing (makeLands)

import Types exposing (..)
import Random exposing (Seed)
import Utils.SharedUtils exposing (..)
import Math.Vector2 as V exposing (Vec2)


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
