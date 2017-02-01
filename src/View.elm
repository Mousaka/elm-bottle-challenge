module View exposing (view)

import Types exposing (Model(..), Action, GameState, Msg(..), Bottle, bottleText, totalVolume)
import ChallengeCreator.Types exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as HA exposing (autofocus, value, min, max)
import Html.Events exposing (onClick, onInput)
import Tuple exposing (first)


view : Model -> Html Types.Msg
view model =
    case model of
        ChallengeCreator gameSeed ->
            div []
                [ div []
                    [ text "Start game with target value "
                    , input
                        [ HA.min "1"
                        , HA.max "23"
                        , value <| toString <| gameSeed.targetValue
                        , onInput (\s -> ChallengeCreatorMsg (Target s))
                        ]
                        []
                    ]
                , div []
                    [ text " and first bottle with volume of "
                    , input
                        [ HA.min "1"
                        , HA.max "23"
                        , value <| toString <| gameSeed.bottle1Size
                        , onInput (\s -> ChallengeCreatorMsg (Bottle1 s))
                        ]
                        []
                    ]
                , div []
                    [ text " and second with "
                    , input
                        [ HA.min "1"
                        , HA.max "23"
                        , value <| toString <| gameSeed.bottle2Size
                        , onInput (\s -> ChallengeCreatorMsg (Bottle2 s))
                        ]
                        []
                    ]
                , button [ autofocus True, onClick (ChallengeCreatorMsg (Create gameSeed)) ] [ text "Start" ]
                ]

        WinScreen actionTrail ->
            winView actionTrail

        Game state ->
            gameView state


winView : List ( Action, Types.BottlePair ) -> Html Types.Msg
winView actions =
    div []
        [ h1 [] [ text ("You solved the challenge in " ++ toString (List.length actions) ++ " steps: ") ]
        , actionList actions
        , button [ onClick PlayAgain, autofocus True ] [ text "Again!" ]
        ]


actionList : List ( Action, Types.BottlePair ) -> Html Types.Msg
actionList actions =
    let
        actionText =
            Tuple.first >> Types.actionText

        stateText =
            Tuple.second >> bottlePairStatus

        toLi =
            Html.text >> (flip (::)) [] >> li []

        trailMapper elem =
            (actionText elem ++ "  ->  " ++ stateText elem) |> toLi
    in
        ol [] <| List.map trailMapper actions


gameView : GameState -> Html Types.Msg
gameView state =
    let
        target =
            (toString state.target)

        bottles =
            state.bottlePairTrailed.bottlePair

        totalVol =
            toString <| totalVolume bottles
    in
        div []
            [ h2 [] [ text ("Target: " ++ target ++ "  | Current total volume: " ++ totalVol) ]
            , bottleStatus bottles
            , actionBar
            ]


bottlePairStatus : Types.BottlePair -> String
bottlePairStatus ( b1, b2 ) =
    bottleText b1 ++ ", " ++ bottleText b2 ++ ", volume: " ++ (toString (totalVolume ( b1, b2 )))


bottleStatus : Types.BottlePair -> Html Types.Msg
bottleStatus ( bottle1, bottle2 ) =
    div []
        [ h2 [] [ text (bottleText bottle1 ++ " <- Operating on") ]
        , h3 [] [ text (bottleText bottle2) ]
        ]


actionBar : Html Types.Msg
actionBar =
    div [] <| List.map actionButton Types.allActions


actionButton : ( Action, String ) -> Html Types.Msg
actionButton ( action, description ) =
    button [ onClick (AnAction action) ] [ Html.text description ]
