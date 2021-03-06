module Rocket.Updates exposing (..)

import Rocket.Types exposing (..)
import Rocket.Movement exposing (..)
import Rocket.Collision exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Scene exposing (drawScene)
import Time exposing (Time, second)
import List.Extra exposing (updateIf, last, init)
import Debug


updateInfo : Msg -> InfoData -> InfoData
updateInfo msg ({ accelerateAnimation, rotateAnimation, landAnimation } as data) =
    { data
        | accelerateAnimation = updateAccelerateAnimation msg accelerateAnimation
        , rotateAnimation = updateRotateAnimation msg rotateAnimation
        , landAnimation = updateLandAnimation msg landAnimation
    }


updateAccelerateAnimation : Msg -> PortraitData -> PortraitData
updateAccelerateAnimation msg ({ rocket } as data) =
    let
        updatedRocket =
            { rocket | fire = not rocket.fire }
    in
        case msg of
            TimerTick ->
                { data | rocket = updatedRocket }

            _ ->
                data


updateRotateAnimation : Msg -> PortraitData -> PortraitData
updateRotateAnimation msg ({ rocket } as data) =
    let
        updatedRocket =
            if abs rocket.angle < 60 then
                { rocket
                    | angle = rocket.angle + rocket.twist
                }
            else
                { rocket
                    | twist = -rocket.twist
                    , angle = rocket.angle - rocket.twist
                }
    in
        case msg of
            Step _ ->
                { data | rocket = updatedRocket }

            _ ->
                data


updateLandAnimation : Msg -> PortraitData -> PortraitData
updateLandAnimation msg ({ rocket, platform } as data) =
    let
        jPlatform =
            case platform of
                Just p ->
                    p

                Nothing ->
                    Debug.crash "LandAnimation should have a platform"

        updatedRocket =
            case rocket.movement of
                Flying ->
                    case msg of
                        Step _ ->
                            if (snd rocket.position) - 5 <= snd jPlatform.center then
                                { rocket | movement = Landing jPlatform }
                            else
                                { rocket
                                    | position = ( fst rocket.position, snd rocket.position - 0.5 )
                                }

                        _ ->
                            rocket

                Landing p ->
                    case msg of
                        TimerTick ->
                            { rocket | movement = Landed p }

                        _ ->
                            rocket

                Landed p ->
                    case msg of
                        TimerTick ->
                            { rocket
                                | movement = Flying
                                , position = ((initInfo.landAnimation).rocket).position
                            }

                        _ ->
                            rocket

                _ ->
                    rocket

        updatedPlatform =
            case rocket.movement of
                Landing p ->
                    Just { jPlatform | marked = True }

                Flying ->
                    Just { jPlatform | marked = False }

                _ ->
                    platform
    in
        { data | rocket = updatedRocket, platform = updatedPlatform }


updateWorldChoice : Msg -> WorldChoiceData -> WorldChoiceData
updateWorldChoice msg ({ worldChoice } as data) =
    case msg of
        KeyUpMsg Right ->
            { data | worldChoice = rotateRight worldChoice }

        KeyUpMsg Left ->
            { data | worldChoice = rotateLeft worldChoice }

        _ ->
            data


updatePlay : Msg -> PlayData -> PlayData
updatePlay msg ({ world, rocket, keyDown } as data) =
    case msg of
        KeyDownMsg key ->
            { data | keyDown = updateKeyDown keyDown (KeyDownMsg key) }

        KeyUpMsg key ->
            { data | keyDown = updateKeyDown keyDown (KeyUpMsg key) }

        Step diffTime ->
            if not (isWin data) then
                case rocket.movement of
                    Landed _ ->
                        updateLanded data

                    Landing _ ->
                        updateLanding data

                    Colliding ->
                        updateColliding data

                    Flying ->
                        updateFlying data diffTime
            else
                { data | playEvent = Just (Win data) }

        TimerTick ->
            if data.timeRemaining <= 0 * second then
                { data | playEvent = Just (Gameover data) }
            else
                { data | timeRemaining = data.timeRemaining - 1 * second }


isWin : PlayData -> Bool
isWin { world } =
    world.isWin world.platforms


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
                    updateDisplayPosition { data | rocket = movedRocket }


updateColliding : PlayData -> PlayData
updateColliding data =
    { data | playEvent = Just (Gameover data) }


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
    if keyDown.up then
        { data | rocket = startRocket rocket }
    else
        data


updateDisplayPosition : PlayData -> PlayData
updateDisplayPosition ({ displayPosition, displaySize, world, rocket } as data) =
    let
        ( dW, dH ) =
            displaySize

        ( wW, wH ) =
            world.size

        tolerancex =
            dW // 4

        tolerancey =
            dH // 4

        ( rx, ry ) =
            rocket.position

        ( rrx, rry ) =
            ( round rx, round ry )

        fokusRocket =
            \( dx, dy ) ->
                let
                    dx' =
                        dx + max 0 (rrx - dx - tolerancex) + min 0 (rrx - dx + tolerancex)

                    dy' =
                        dy + max 0 (rry - dy - tolerancey) + min 0 (rry - dy + tolerancey)
                in
                    ( dx', dy' )

        stayInFrame =
            \( dx, dy ) ->
                let
                    -- adjust left
                    dx' =
                        max (round (-wW / 2) + dW // 2) dx

                    -- adjust right
                    dx'' =
                        min (round (wW / 2) - dW // 2) dx'

                    -- adjust bottom
                    dy' =
                        max (round (-wH / 2) + dH // 2) dy

                    -- adjust top
                    dy'' =
                        min (round (wH / 2) - dH // 2) dy'
                in
                    ( dx'', dy'' )
    in
        { data | displayPosition = stayInFrame <| fokusRocket displayPosition }


updateKeyDown : KeyDown -> Msg -> KeyDown
updateKeyDown keyDown msg =
    case msg of
        KeyDownMsg key ->
            case key of
                Left ->
                    { keyDown | left = True }

                Right ->
                    { keyDown | right = True }

                Up ->
                    { keyDown | up = True }

                _ ->
                    keyDown

        KeyUpMsg key ->
            case key of
                Left ->
                    { keyDown | left = False }

                Right ->
                    { keyDown | right = False }

                Up ->
                    { keyDown | up = False }

                _ ->
                    keyDown

        _ ->
            Debug.crash "updateKeyDown called with neither KeyDownMsg nor KeyUpMsg"


markPlatform : Platform -> List Platform -> List Platform
markPlatform p ps =
    updateIf ((==) p) (\p' -> { p' | marked = True }) ps


rotateRight : List a -> List a
rotateRight xs =
    case xs of
        [] ->
            []

        x :: xs' ->
            xs' ++ [ x ]


rotateLeft : List a -> List a
rotateLeft xs =
    case xs of
        [] ->
            []

        xs ->
            let
                x' =
                    case last xs of
                        Just x ->
                            x

                        Nothing ->
                            Debug.crash "List should not be empty here"

                xs' =
                    case init xs of
                        Just xs ->
                            xs

                        Nothing ->
                            Debug.crash "List should not be empty here"
            in
                x' :: xs'
