module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Types exposing (..)
import State exposing (..)
import Tuple exposing (second, first)


emptyPair : Int -> Int -> BottlePair
emptyPair b1 b2 =
    ( { capacity = b1, volume = 0 }, { capacity = b2, volume = 0 } )


emptyBottlePair : BottlePairTrailed
emptyBottlePair =
    BottlePairTrailed empty53 []


empty53 : BottlePair
empty53 =
    emptyPair 5 3


fullLeftPair : BottlePair
fullLeftPair =
    ( { capacity = 5, volume = 5 }, { capacity = 3, volume = 0 } )


fullRightPair : BottlePair
fullRightPair =
    ( { capacity = 5, volume = 0 }, { capacity = 3, volume = 3 } )


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Big" <|
                \() ->
                    Expect.equal (first (minStep 1 5 3)) 1
            , test "Big2" <|
                \() ->
                    [ Swap, FillLeft, PourAllToRight, EmptyLeft, FillLeft, PourUntilFull, Swap, EmptyLeft ]
                        |> performeActions emptyBottlePair
                        |> bottlePairTrailedVolume
                        |> Expect.equal 1
            , test "swap" <|
                \() -> Expect.equal ( second empty53, first empty53 ) <| .bottlePair <| swap <| emptyBottlePair
            , test "Pour all to right" <|
                \() -> Expect.equal fullRightPair <| .bottlePair <| pourAllToRight <| BottlePairTrailed fullLeftPair []
            , test "Pour until full to right" <|
                \() -> Expect.equal ( { capacity = 5, volume = 2 }, { capacity = 3, volume = 3 } ) <| .bottlePair <| pourUntilFull <| BottlePairTrailed fullLeftPair []
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list (Fuzz.intRange 1 5)) "Very naive and crazy (and bad) approach to find the solution with a property test that never expects the shortest answer to show up - that does not work" <|
                \aList ->
                    aList
                        |> toActions
                        |> performeActions emptyBottlePair
                        |> theCorrectResult
                        |> Expect.false "theCorrectResult"
            ]
        ]


theCorrectResult : BottlePairTrailed -> Bool
theCorrectResult b =
    bottlePairTrailedVolume b == 1 && (List.length b.trails) == 6


toActions : List Int -> List Action
toActions =
    List.map intToAction


intToAction : Int -> Action
intToAction b =
    case b of
        1 ->
            FillLeft

        2 ->
            Swap

        3 ->
            PourAllToRight

        4 ->
            PourUntilFull

        _ ->
            EmptyLeft
