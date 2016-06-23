module Rocket.Updates exposing (..)

import Rocket.Types exposing (..)
import Rocket.Movement exposing (..)
import Rocket.Collision exposing (..)
import Time exposing (Time)


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

