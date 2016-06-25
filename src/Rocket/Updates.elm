module Rocket.Updates exposing (..)

import Rocket.Types exposing (..)
import Rocket.Movement exposing (..)
import Rocket.Collision exposing (..)
import Time exposing (Time, second)
import List.Extra exposing (updateIf)
import Debug


updatePlay : Msg -> PlayData -> PlayData
updatePlay msg ({ world, rocket, keyDown } as data) =
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

        TimerTick ->
            if data.timeRemaining <= 0 * second then
                { data | gameover = True }
            else
                { data | timeRemaining = data.timeRemaining - 1 * second }

        _ ->
            data


updateFlying : PlayData -> Time -> PlayData
updateFlying ({ world, rocket, keyDown } as data) diffTime =
    let
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
updateLanding ({ world, rocket } as data) =
    case rocket.movement of
        Landing platform ->
            { data
                | rocket = landOn platform rocket
                , world =
                    { world
                        | platforms = markPlatform platform world.platforms
                    }
            }

        _ ->
            Debug.crash "update Landing called with non \"Landing ...\" Msg"


updateLanded : PlayData -> PlayData
updateLanded ({ rocket, keyDown } as data) =
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
            Debug.crash "updateKeyDown called with neither KeyDownMsg nor KeyUpMsg"


markPlatform : Platform -> List Platform -> List Platform
markPlatform p ps =
    updateIf ((==) p) (\p' -> { p' | marked = True }) ps
