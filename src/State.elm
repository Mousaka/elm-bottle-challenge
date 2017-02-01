module State exposing (..)

import Types exposing (..)
import ChallengeCreator.Types exposing (Msg(Create))
import ChallengeCreator.State exposing (update)


init : Model
init =
    ChallengeCreator ChallengeCreator.State.initSeed


updateWithCmd : Types.Msg -> Model -> ( Model, Cmd Types.Msg )
updateWithCmd msg model =
    ( update msg model, Cmd.none )


update : Types.Msg -> Model -> Model
update msg model =
    case msg of
        ChallengeCreatorMsg ccMsg ->
            challengeUpdate ccMsg model

        PlayAgain ->
            init

        AnAction action ->
            case model of
                Game state ->
                    let
                        newBottlePairTrailed =
                            performeAction action state.bottlePairTrailed

                        newGameState =
                            { state | bottlePairTrailed = newBottlePairTrailed }
                    in
                        if reachedTarget newGameState then
                            WinScreen newBottlePairTrailed.trails
                        else
                            Game newGameState

                _ ->
                    model


challengeUpdate : ChallengeCreator.Types.Msg -> Model -> Model
challengeUpdate msg model =
    case msg of
        Create gameSeed ->
            Game (initGame gameSeed)

        somemsg ->
            case model of
                ChallengeCreator gameSeed ->
                    ChallengeCreator (ChallengeCreator.State.update somemsg gameSeed)

                _ ->
                    model


reachedTarget : GameState -> Bool
reachedTarget gameState =
    gameState.target == totalVolume gameState.bottlePairTrailed.bottlePair


emptyLeft : BottlePairTrailed -> BottlePairTrailed
emptyLeft bottlePairTrailed =
    let
        { bottlePair, trails } =
            bottlePairTrailed

        ( b1, b2 ) =
            bottlePair

        newB1 =
            { b1 | volume = 0 }
    in
        if b1.volume == 0 then
            bottlePairTrailed
        else
            BottlePairTrailed ( newB1, b2 ) (trails ++ [ ( EmptyLeft, ( newB1, b2 ) ) ])


pourAllToRight : BottlePairTrailed -> BottlePairTrailed
pourAllToRight bottlePairTrailed =
    let
        ( b1, b2 ) =
            bottlePairTrailed.bottlePair

        newB2 =
            if totalVolume ( b1, b2 ) >= b2.capacity then
                { b2 | volume = b2.capacity }
            else
                { b2 | volume = (totalVolume ( b1, b2 )) }

        trails =
            bottlePairTrailed.trails
                ++ if b1.volume == 0 then
                    []
                   else
                    [ ( PourAllToRight, ( b1, newB2 ) ) ]
    in
        BottlePairTrailed ( { b1 | volume = 0 }, newB2 ) trails


swap : BottlePairTrailed -> BottlePairTrailed
swap bottlePairTrailed =
    let
        ( b1, b2 ) =
            bottlePairTrailed.bottlePair

        swappedPair =
            ( b2, b1 )

        newTrails =
            bottlePairTrailed.trails ++ [ ( Swap, swappedPair ) ]
    in
        { bottlePairTrailed | bottlePair = swappedPair, trails = newTrails }


pourUntilFull : BottlePairTrailed -> BottlePairTrailed
pourUntilFull bottlePairTrailed =
    let
        ( b1, b2 ) =
            bottlePairTrailed.bottlePair

        bottlePair =
            if totalVolume ( b1, b2 ) >= b2.capacity then
                ( { b1 | volume = b1.volume - (b2.capacity - b2.volume) }, { b2 | volume = b2.capacity } )
            else
                ( { b1 | volume = 0 }, { b2 | volume = totalVolume ( b1, b2 ) } )

        trails =
            bottlePairTrailed.trails
                ++ if b1.volume == 0 then
                    []
                   else
                    [ ( PourUntilFull, bottlePair ) ]
    in
        BottlePairTrailed bottlePair trails


fillMax : Bottle -> Bottle
fillMax bottle =
    { bottle | volume = bottle.capacity }


fillLeft : BottlePairTrailed -> BottlePairTrailed
fillLeft bottlePairTrailed =
    let
        ( b1, b2 ) =
            bottlePairTrailed.bottlePair

        newBottlePair =
            ( (fillMax b1), b2 )

        trails =
            if b1.volume == b1.capacity then
                bottlePairTrailed.trails
            else
                bottlePairTrailed.trails ++ [ ( FillLeft, newBottlePair ) ]
    in
        BottlePairTrailed newBottlePair trails



-- Target 1, 4        bottles: 5, 3


minStep : Int -> Int -> Int -> ( Int, BottlePairTrailed )
minStep target bottle1 bottle2 =
    let
        bottles : BottlePair
        bottles =
            ( { capacity = bottle1, volume = 0 }, { capacity = bottle2, volume = 0 } )

        bottlePairTrailed =
            BottlePairTrailed bottles []
                |> swap
                |> fillLeft
                |> pourAllToRight
                |> emptyLeft
                |> fillLeft
                |> pourUntilFull
                |> swap
                |> emptyLeft
    in
        ( totalVolume bottlePairTrailed.bottlePair, bottlePairTrailed )


bottlePairTrailedVolume : BottlePairTrailed -> Int
bottlePairTrailedVolume =
    .bottlePair >> totalVolume


performeActions : BottlePairTrailed -> List Action -> BottlePairTrailed
performeActions bottlePairTrailed =
    List.foldl performeAction bottlePairTrailed


performeAction : Action -> BottlePairTrailed -> BottlePairTrailed
performeAction action =
    case action of
        Swap ->
            swap

        FillLeft ->
            fillLeft

        PourAllToRight ->
            pourAllToRight

        PourUntilFull ->
            pourUntilFull

        EmptyLeft ->
            emptyLeft
