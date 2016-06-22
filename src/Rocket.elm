module Rocket exposing (..)

import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Movement exposing (..)
import Rocket.Views exposing (..)
import Rocket.Collision exposing (collision)
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


updatePlay : Msg -> PlayData -> PlayData
updatePlay msg data =
    let
        world =
            data.world

        rocket =
            data.rocket

        keyDown =
            data.keyDown
    in
        case msg of
            KeyDownMsg key ->
                { data | keyDown = updateKeyDown keyDown (KeyDownMsg key) }

            KeyUpMsg key ->
                { data | keyDown = updateKeyDown keyDown (KeyUpMsg key) }

            Step diffTime ->
                case rocket.movement of
                    Landed _ ->
                        updateLanded data

                    Landing _ ->
                        updateLanding data

                    Colliding ->
                        updateColliding data

                    Flying ->
                        updateFlying data diffTime

            _ ->
                data


updateFlying : PlayData -> Time -> PlayData
updateFlying data diffTime =
    let
        world =
            data.world

        rocket =
            data.rocket

        keyDown =
            data.keyDown

        movedRocket =
            moveRocket keyDown
                world.gravity
                diffTime
                rocket
    in
        case tryLanding movedRocket world of
            Just platform ->
                { data
                    | rocket =
                        { movedRocket | movement = Landing platform }
                }

            Nothing ->
                if collision movedRocket world then
                    { data
                        | rocket =
                            { movedRocket | movement = Colliding }
                    }
                else
                    { data | rocket = movedRocket }


updateColliding : PlayData -> PlayData
updateColliding data =
    { data | gameover = True }


updateLanding : PlayData -> PlayData
updateLanding data =
    let
        world =
            data.world

        platforms =
            world.platforms

        rocket =
            data.rocket
    in
        case rocket.movement of
            Landing platform ->
                { data
                    | rocket = landOn platform rocket
                    , world =
                        { world
                            | platforms = markPlatform platform platforms
                        }
                }

            -- others should not happen when calling updateLanding
            _ ->
                data


updateLanded : PlayData -> PlayData
updateLanded data =
    let
        keyDown =
            data.keyDown

        rocket =
            data.rocket
    in
        if keyDown.forward then
            { data | rocket = startRocket rocket }
        else
            data


update : Msg -> Model -> Model
update msg model =
    case model of
        Play data ->
            let
                updatedData =
                    updatePlay msg data
            in
                if updatedData.gameover then
                    Gameover initGameover
                else
                    Play updatedData

        _ ->
            case msg of
                KeyUpMsg Start ->
                    Play initPlay

                _ ->
                    model


updateKeyDown : KeyDown -> Msg -> KeyDown
updateKeyDown keyDown msg =
    case msg of
        KeyDownMsg key ->
            case key of
                Left ->
                    { keyDown | left = True }

                Right ->
                    { keyDown | right = True }

                Forward ->
                    { keyDown | forward = True }

                _ ->
                    keyDown

        KeyUpMsg key ->
            case key of
                Left ->
                    { keyDown | left = False }

                Right ->
                    { keyDown | right = False }

                Forward ->
                    { keyDown | forward = False }

                _ ->
                    keyDown

        _ ->
            keyDown


markPlatform : Platform -> List Platform -> List Platform
markPlatform p ps =
    case ps of
        [] ->
            []

        p' :: ps' ->
            if p == p' then
                { p | marked = True } :: ps'
            else
                p' :: markPlatform p ps'


main =
    program
        { init = ( init, Cmd.none )
        , view = view
        , update = \msg playModel -> ( update msg playModel, Cmd.none )
        , subscriptions = subscriptions
        }
