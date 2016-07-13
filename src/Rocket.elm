module Rocket exposing (..)

import Rocket.Types exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Updates exposing (..)
import Rocket.Views exposing (..)
import Rocket.Scene exposing (drawScene)
import Html exposing (Html, text, div, br, h3)
import Html.App exposing (program)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard
import Char exposing (KeyCode, fromCode)
import Debug
import Rocket.Audio exposing (audio)


keyBinding : Model -> KeyCode -> Key
keyBinding model =
    case model of
        WorldChoice _ ->
            \code ->
                case fromCode code of
                    'A' ->
                        Left

                    'D' ->
                        Right

                    ' ' ->
                        Start

                    _ ->
                        NotBound

        Play _ ->
            \code ->
                case fromCode code of
                    'W' ->
                        Up

                    'A' ->
                        Left

                    'D' ->
                        Right

                    _ ->
                        NotBound

        _ ->
            \code ->
                case fromCode code of
                    ' ' ->
                        Start

                    _ ->
                        NotBound


view : Model -> Html a
view model =
    div []
        [ case model of
            Play data ->
                viewPlay data

            StartScreen ->
                viewStartScreen

            WorldChoice data ->
                viewWorldChoice data

            Gameover data ->
                viewGameover data

            Win data ->
                viewWin data
        , h3 [] [ text "Short Introduction:" ]
        , text "Control the rocket with W (accelerate), A (turn left) and D (turn right)."
        , br [] []
        , text "Try to land on all platforms in the given time."
        , br [] []
        , text "(Visited platforms turn from red to green)."
        , br [] []
        , text "The rocket must have an apropriate angle and speed when landing."
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( case model of
        StartScreen ->
            case msg of
                KeyUpMsg Start ->
                    WorldChoice initWorldChoice

                _ ->
                    model

        WorldChoice data ->
            case msg of
                KeyUpMsg Start ->
                    case List.head data.worlds of
                        Just world ->
                            Play (startPlay world)

                        Nothing ->
                            Debug.crash "No world found"

                KeyUpMsg direction ->
                    WorldChoice (updateWorldChoice direction data)

                _ ->
                    model

        Play data ->
            let
                updatedPlay =
                    updatePlay msg data
            in
                if isGameover updatedPlay then
                    Gameover
                        { initGameover
                            | background =
                                drawScene updatedPlay
                        }
                else if isWin updatedPlay then
                    Win
                        { initWin
                            | background =
                                drawScene updatedPlay
                        }
                else
                    Play updatedPlay

        Gameover _ ->
            case msg of
                KeyUpMsg Start ->
                    StartScreen

                _ ->
                    model

        Win _ ->
            case msg of
                KeyUpMsg Start ->
                    StartScreen

                _ ->
                    model
    , case model of
        Play data ->
            if data.rocket.fire then
                audio ( "rocket", "play" )
            else
                Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]

        _ ->
            Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]
    )


startPlay : World -> PlayData
startPlay world =
    { initPlay
        | world = world
        , timeRemaining = world.totalTime
        , rocket =
            { initRocket
                | position = world.rocketStartPosition
            }
    }


isGameover : PlayData -> Bool
isGameover data =
    data.gameover


isWin : PlayData -> Bool
isWin { world } =
    world.isWin world.platforms


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        <| [ Keyboard.downs (KeyDownMsg << keyBinding model)
           , Keyboard.ups (KeyUpMsg << keyBinding model)
           ]
        ++ case model of
            Play _ ->
                [ diffs Step
                , every second <| always TimerTick
                ]

            _ ->
                []


main : Program Never
main =
    program
        { init = ( init, audio ( "ambient", "play" ) )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
