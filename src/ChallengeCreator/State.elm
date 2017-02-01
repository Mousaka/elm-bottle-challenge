module ChallengeCreator.State exposing (initSeed, update)

import ChallengeCreator.Types exposing (GameSeed, Msg(..))


initSeed : GameSeed
initSeed =
    { targetValue = 1, bottle1Size = 3, bottle2Size = 5 }


update : Msg -> GameSeed -> GameSeed
update msg model =
    case msg of
        Target value ->
            { model | targetValue = toIntWithDefault value }

        Bottle1 value ->
            { model | bottle1Size = toIntWithDefault value }

        Bottle2 value ->
            { model | bottle2Size = toIntWithDefault value }

        _ ->
            model


toIntWithDefault : String -> Int
toIntWithDefault value =
    Result.withDefault 0 (String.toInt value)
