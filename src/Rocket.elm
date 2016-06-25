module Rocket exposing (..)

import Rocket.Types exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Updates exposing (..)
import Rocket.Views exposing (..)
import Rocket.Scene exposing (drawScene)
import Html exposing (Html)
import Html.App exposing (program)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard
import Char exposing (KeyCode, fromCode)


keyBinding : Model -> KeyCode -> Key
keyBinding model =
    case model of
        Play _ ->
            \code ->
                case fromCode code of
                    'W' ->
                        Forward

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
    case model of
        Play data ->
            viewPlay data

        Startscreen data ->
            viewStartscreen data

        Gameover data ->
            viewGameover data

        Win data ->
            viewWin data


update : Msg -> Model -> Model
update msg model =
    case model of
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

        _ ->
            case msg of
                KeyUpMsg Start ->
                    Play initPlay

                _ ->
                    model


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
        { init = ( init, Cmd.none )
        , view = view
        , update = \msg playModel -> ( update msg playModel, Cmd.none )
        , subscriptions = subscriptions
        }
