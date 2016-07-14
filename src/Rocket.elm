module Rocket exposing (..)

import Rocket.Types exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Updates exposing (..)
import Rocket.Views exposing (..)
import Rocket.Scene exposing (drawScene)
import Html exposing (Html, text, div, br, h3, body)
import Html.Attributes exposing (id)
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

        'M' ->
            AmbientMusic

        _ ->
            NotBound


isScreenControlKey : Key -> Bool
isScreenControlKey key =
    case key of
        Start ->
            True

        Back ->
            True

        AmbientMusic ->
            True

        _ ->
            False


view : Model -> Html a
view (Model screen options) =
    div []
        [ div [ id "gameScreen" ]
            [ viewScreen screen ]
        , div [ id "gameControl" ]
            [ h3 [] [ text "Short Introduction:" ]
            , text "Control the rocket with W (accelerate), A (turn left) and D (turn right)."
            , br [] []
            , text "Try to land on all platforms in the given time."
            , br [] []
            , text "(Visited platforms turn from red to green)."
            , br [] []
            , text "The rocket must have an apropriate angle and speed when landing."
            ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        updatedModel =
            updateModel msg model
    in
        ( updatedModel, sendCmd updatedModel )


updateModel : Msg -> Model -> Model
updateModel msg ((Model screen options) as model) =
    if isScreenControlMsg msg then
        changeScreen msg model
    else
        case screen of
            PlayScreen data ->
                case data.playEvent of
                    Nothing ->
                        Model (PlayScreen (updatePlay msg data)) options

                    Just (Gameover data) ->
                        let
                            background =
                                drawScene data
                        in
                            Model (GameoverScreen { initGameover | background = background }) options

                    Just (Win data) ->
                        let
                            background =
                                drawScene data
                        in
                            Model (WinScreen { initWin | background = background }) options

            WorldChoiceScreen data ->
                Model (WorldChoiceScreen (updateWorldChoice msg data)) options

            InfoScreen data ->
                Model (InfoScreen (updateInfo msg data)) options

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
                    Model (InfoScreen initInfo) options

                InfoScreen _ ->
                    changeToWorldChoice options

                WorldChoiceScreen data ->
                    changeToPlay { options | worldChoice = data.worldChoice }

                PlayScreen _ ->
                    model

                GameoverScreen _ ->
                    changeToPlay options

                WinScreen _ ->
                    changeToWorldChoice options

        KeyUpMsg Back ->
            case screen of
                StartScreen ->
                    model

                InfoScreen _ ->
                    Model StartScreen options

                WorldChoiceScreen _ ->
                    Model (InfoScreen initInfo) options

                PlayScreen _ ->
                    changeToWorldChoice options

                GameoverScreen _ ->
                    changeToWorldChoice options

                WinScreen _ ->
                    changeToWorldChoice options

        KeyUpMsg AmbientMusic ->
            Model screen { options | ambientMusic = not options.ambientMusic }

        _ ->
            model


changeToWorldChoice : Options -> Model
changeToWorldChoice options =
    Model (WorldChoiceScreen { initWorldChoice | worldChoice = options.worldChoice }) options


changeToPlay : Options -> Model
changeToPlay ({ worldChoice } as options) =
    let
        world =
            case List.head worldChoice of
                Just w ->
                    w

                Nothing ->
                    Debug.crash "No world found"

        playData =
            { initPlay
                | world = world
                , timeRemaining = world.totalTime
                , rocket =
                    { initRocket
                        | position = world.rocketStartPosition
                    }
            }
    in
        Model (PlayScreen playData) options


sendCmd : Model -> Cmd msg
sendCmd ((Model screen options) as model) =
    Cmd.batch
        [ if options.ambientMusic then
            audio ( "ambient", "play" )
          else
            audio ( "ambient", "pause" )
        , case screen of
            PlayScreen data ->
                if data.rocket.fire then
                    audio ( "rocket", "play" )
                else
                    Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]

            _ ->
                Cmd.batch [ audio ( "rocket", "pause" ), audio ( "rocket", "reset" ) ]
        ]


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

            InfoScreen _ ->
                [ diffs Step
                , every second <| always TimerTick
                ]

            _ ->
                []


main : Program Never
main =
    program
        { init = ( initModel, sendCmd initModel )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
