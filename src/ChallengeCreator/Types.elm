module ChallengeCreator.Types exposing (GameSeed, Msg(..))


type Msg
    = Create GameSeed
    | Target String
    | Bottle1 String
    | Bottle2 String


type alias GameSeed =
    { targetValue : Int
    , bottle1Size : Int
    , bottle2Size : Int
    }
