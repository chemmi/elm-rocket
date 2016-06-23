module Rocket exposing (..)

import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Updates exposing (..)
import Rocket.Views exposing (..)
import Rocket.Scene exposing (drawScene)
import Keyboard
import Char exposing (KeyCode, fromCode)
import AnimationFrame exposing (..)
import Time exposing (..)


keyBinding : Model -> KeyCode -> Key
keyBinding model code =
    case model of
        Play _ ->
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
            case fromCode code of
                ' ' ->
                    Start

                _ ->
                    NotBound


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
                                drawScene updatedPlay.world
                                    updatedPlay.rocket
                        }
                else if isWin updatedPlay then
                    Win
                        { initWin
                            | background =
                                drawScene updatedPlay.world
                                    updatedPlay.rocket
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
isWin data =
    let
        world =
            data.world

        platforms =
            world.platforms
    in
        world.isWin platforms


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ Keyboard.downs (KeyDownMsg << (keyBinding model))
         , Keyboard.ups (KeyUpMsg << (keyBinding model))
         ]
            ++ case model of
                Play _ ->
                    [ diffs Step
                      --, every (playModel.updateInterval * millisecond) (\_ -> Step)
                    ]

                _ ->
                    []
        )


main =
    program
        { init = ( init, Cmd.none )
        , view = view
        , update = \msg playModel -> ( update msg playModel, Cmd.none )
        , subscriptions = subscriptions
        }
