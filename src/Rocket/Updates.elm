module Rocket.Updates exposing (..)

import Rocket.Types exposing (..)
import Rocket.Movement exposing (..)
import Rocket.Collision exposing (..)
import Rocket.Inits exposing (..)
import Rocket.Scene exposing (drawScene)
import Time exposing (Time, second)
import List.Extra exposing (updateIf, last, init)
import Debug


updateScreen : Msg -> Screen -> Screen
updateScreen msg screen =
    case screen of
        StartScreen ->
            case msg of
                KeyUpMsg Start ->
                    WorldChoiceScreen initWorldChoiceScreen

                _ ->
                    screen

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
                    screen

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
                    screen

        WinScreen _ ->
            case msg of
                KeyUpMsg Start ->
                    StartScreen

                _ ->
                    screen


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


updateWorldChoiceScreen : Key -> WorldChoiceData -> WorldChoiceData
updateWorldChoiceScreen key ({ worlds } as data) =
    case key of
        Right ->
            { data | worlds = rotateRight worlds }

        Left ->
            { data | worlds = rotateLeft worlds }

        _ ->
            data


updatePlayScreen : Msg -> PlayData -> PlayData
updatePlayScreen msg ({ world, rocket, keyDown } as data) =
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
                    { keyDown | forward = True }

                _ ->
                    keyDown

        KeyUpMsg key ->
            case key of
                Left ->
                    { keyDown | left = False }

                Right ->
                    { keyDown | right = False }

                Up ->
                    { keyDown | forward = False }

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
