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
import List


keyBinding : KeyCode -> Key
keyBinding code =
    case fromCode code of
        'A' ->
            Left

        'D' ->
            Right

        'W' ->
            Up

        'S' ->
            Down

        ' ' ->
            Start

        'B' ->
            Back

        _ ->
            NotBound


isScreenControlKey : Key -> Bool
isScreenControlKey key =
    case key of
        Start ->
            True

        Back ->
            True

        _ ->
            False


view : Model -> Html a
view (Model screen options) =
    div []
        [ viewScreen screen
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
    ( updateModel msg model, sendCmd msg model )


updateModel : Msg -> Model -> Model
updateModel msg ((Model screen options) as model) =
    if isScreenControlMsg msg then
        changeScreen msg model
    else
        case screen of
            PlayScreen data ->
                case data.playEvent of
                    Nothing ->
                        Model (PlayScreen (updatePlayScreen msg data)) options

                    Just (Gameover data) ->
                        let
                            background =
                                drawScene data
                        in
                            Model (GameoverScreen { initGameoverScreen | background = background }) options

                    Just (Win data) ->
                        let
                            background =
                                drawScene data
                        in
                            Model (WinScreen { initWinScreen | background = background }) options

            WorldChoiceScreen data ->
                Model (WorldChoiceScreen (updateWorldChoiceScreen msg data)) options

            _ ->
                model


isScreenControlMsg : Msg -> Bool
isScreenControlMsg msg =
    case msg of
        KeyUpMsg key ->
            isScreenControlKey key

        _ ->
            False


changeScreen : Msg -> Model -> Model
changeScreen msg ((Model screen options) as model) =
    case msg of
        KeyUpMsg Start ->
            case screen of
                StartScreen ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

                WorldChoiceScreen data ->
                    case List.head data.worlds of
                        Just world ->
                            Model (PlayScreen (startPlayScreen world)) options

                        Nothing ->
                            Debug.crash "No world found"

                GameoverScreen _ ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

                WinScreen _ ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

                PlayScreen _ ->
                    model

        KeyUpMsg Back ->
            case screen of
                StartScreen ->
                    model

                WorldChoiceScreen _ ->
                    Model StartScreen options

                GameoverScreen _ ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

                WinScreen _ ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

                PlayScreen _ ->
                    Model (WorldChoiceScreen initWorldChoiceScreen) options

        _ ->
            model


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


sendCmd : Msg -> Model -> Cmd msg
sendCmd msg ((Model screen options) as model) =
    case screen of
        PlayScreen data ->
            if data.rocket.fire then
                audio ( "rocket", "play" )
            else
                Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]

        _ ->
            Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]


subscriptions : Model -> Sub Msg
subscriptions ((Model screen options) as model) =
    Sub.batch
        <| [ Keyboard.downs (KeyDownMsg << keyBinding)
           , Keyboard.ups (KeyUpMsg << keyBinding)
           ]
        ++ case screen of
            PlayScreen _ ->
                [ diffs Step
                , every second <| always TimerTick
                ]

            _ ->
                []


main : Program Never
main =
    program
        { init = ( initModel, audio ( "ambient", "play" ) )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
