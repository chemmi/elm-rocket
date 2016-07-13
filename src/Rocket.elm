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
        WorldChoiceScreen _ ->
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

        PlayScreen _ ->
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
            PlayScreen data ->
                viewPlayScreen data

            StartScreen ->
                viewStartScreen

            WorldChoiceScreen data ->
                viewWorldChoiceScreen data

            GameoverScreen data ->
                viewGameoverScreen data

            WinScreen data ->
                viewWinScreen data
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
                    WorldChoiceScreen initWorldChoiceScreen

                _ ->
                    model

        WorldChoiceScreen data ->
            case msg of
                KeyUpMsg Start ->
                    case List.head data.worlds of
                        Just world ->
                            PlayScreen (startPlayScreen world)

                        Nothing ->
                            Debug.crash "No world found"

                KeyUpMsg direction ->
                    WorldChoiceScreen (updateWorldChoiceScreen direction data)

                _ ->
                    model

        PlayScreen data ->
            let
                updatedPlayScreen =
                    updatePlayScreen msg data
            in
                if isGameover updatedPlayScreen then
                    GameoverScreen
                        { initGameoverScreen
                            | background =
                                drawScene updatedPlayScreen
                        }
                else if isWin updatedPlayScreen then
                    WinScreen
                        { initWinScreen
                            | background =
                                drawScene updatedPlayScreen
                        }
                else
                    PlayScreen updatedPlayScreen

        GameoverScreen _ ->
            case msg of
                KeyUpMsg Start ->
                    StartScreen

                _ ->
                    model

        WinScreen _ ->
            case msg of
                KeyUpMsg Start ->
                    StartScreen

                _ ->
                    model
    , case model of
        PlayScreen data ->
            if data.rocket.fire then
                audio ( "rocket", "play" )
            else
                Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]

        _ ->
            Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]
    )


startPlayScreen : World -> PlayData
startPlayScreen world =
    { initPlayScreen
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
            PlayScreen _ ->
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
