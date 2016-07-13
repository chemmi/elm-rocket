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
keyBinding (Model screen options) =
    case screen of
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
    ( updateModel msg model
    , sendCmd msg model
    )


updateModel : Msg -> Model -> Model
updateModel msg (Model screen options) =
    case screen of
        PlayScreen data ->
            case data.playEvent of
                Nothing ->
                    Model (updateScreen msg screen) options

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

        _ ->
            Model (updateScreen msg screen) options


sendCmd : Msg -> Model -> Cmd msg
sendCmd msg (Model screen options as model) =
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
        <| [ Keyboard.downs (KeyDownMsg << keyBinding model)
           , Keyboard.ups (KeyUpMsg << keyBinding model)
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
