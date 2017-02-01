module Types exposing (..)

import ChallengeCreator.Types exposing (GameSeed)


type Model
    = ChallengeCreator GameSeed
    | Game GameState
    | WinScreen (List ( Action, BottlePair ))


type alias GameState =
    { target : Int
    , bottlePairTrailed : BottlePairTrailed
    }


type alias BottlePairTrailed =
    { bottlePair : BottlePair
    , trails : List ( Action, BottlePair )
    }


initGame : GameSeed -> GameState
initGame seed =
    GameState seed.targetValue
        { bottlePair = ( (Bottle seed.bottle1Size 0), (Bottle seed.bottle2Size 0) )
        , trails = []
        }


type Msg
    = ChallengeCreatorMsg ChallengeCreator.Types.Msg
    | PlayAgain
    | AnAction Action


allActions : List ( Action, String )
allActions =
    List.map (\a -> ( a, actionText a )) [ FillLeft, Swap, PourAllToRight, PourUntilFull, EmptyLeft ]


type Action
    = FillLeft
    | Swap
    | PourAllToRight
    | PourUntilFull
    | EmptyLeft


type alias BottlePair =
    ( Bottle, Bottle )


actionText : Action -> String
actionText action =
    case action of
        FillLeft ->
            "Fill bottle"

        Swap ->
            "Swap bottle to operate on"

        PourAllToRight ->
            "Pour all to other"

        PourUntilFull ->
            "Pour until other is full"

        EmptyLeft ->
            "Empty bottle"


totalVolume : BottlePair -> Int
totalVolume ( b1, b2 ) =
    .volume b1 + (.volume b2)


type alias Bottle =
    { capacity : Int, volume : Int }


bottleText : Bottle -> String
bottleText bottle =
    let
        volume =
            toString bottle.volume

        capacity =
            toString bottle.capacity
    in
        (capacity ++ "-bottle is filled with " ++ volume)
