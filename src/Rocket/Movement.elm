module Rocket.Movement exposing (..)

import Rocket.Types exposing (..)
import Time exposing (..)
import List exposing (map)
import Maybe exposing (oneOf)


moveRocket : KeyDown -> Float -> Time -> Rocket -> Rocket
moveRocket keyDown gravity diffTime rocket =
    let
        ( x, y ) =
            rocket.position

        angle =
            rocket.angle

        fire =
            rocket.fire

        ( vx, vy ) =
            rocket.velocity

        acceleration =
            rocket.acceleration

        twist =
            rocket.twist

        diffSeconds =
            inSeconds diffTime
    in
        { rocket
            | angle =
                if keyDown.left then
                    angle + twist
                else if keyDown.right then
                    angle - twist
                else
                    angle
            , velocity =
                accelerate (gravity * diffSeconds) 180
                    <| (if keyDown.forward then
                            accelerate (acceleration * diffSeconds) angle
                        else
                            identity
                       )
                    <| ( vx, vy )
            , position = ( vx + x, vy + y )
            , fire = keyDown.forward
        }


startRocket : Rocket -> Rocket
startRocket rocket =
    let
        ( px, py ) =
            rocket.position
    in
        { rocket
            | movement = Flying
            , position = ( px, py + 1 )
            , fire = True
            , onPlatform = Nothing
        }


tryLanding : Rocket -> World -> Maybe Platform
tryLanding rocket world =
    let
        platforms =
            world.platforms

        (( vx, vy ) as v) =
            rocket.velocity

        (( rx, ry ) as pos) =
            rocket.position

        ( b1, b2 ) =
            rocket.base

        ( b1x, b1y ) =
            b1

        ( b2x, b2y ) =
            b2

        roundedAngle =
            round rocket.angle % 360

        angleTolerance =
            10

        vxTolerance =
            1

        vyTolerance =
            2

        landOnPlatform =
            \platform ->
                let
                    ( cx, cy ) =
                        platform.center

                    plathw =
                        platform.width / 2
                in
                    if
                        ((roundedAngle < angleTolerance)
                            || (roundedAngle > 360 - angleTolerance)
                        )
                            && -- coarse (base should be rotated)
                               (b1x + rx > cx - plathw)
                            && -- coarse (base should be rotated)
                               (b2x + rx < cx + plathw)
                            && -- coarse (base should be rotated)
                               ((b1y + ry <= cy && b1y + ry > cy - 10)
                                    || (b2y + ry <= cy && b2y + ry > cy - 10)
                               )
                            && (abs vx < vxTolerance)
                            && (abs vy < vyTolerance)
                    then
                        Just platform
                    else
                        Nothing
    in
        oneOf (map landOnPlatform platforms)


landOn : Platform -> Rocket -> Rocket
landOn platform rocket =
    let
        ( rx, ry ) =
            rocket.position

        ( b1x, b1y ) =
            fst (rocket.base)

        ( cx, cy ) =
            platform.center
    in
        { rocket
            | movement = Landed platform
            , position = ( rx, cy - b1y )
            , velocity = ( 0, 0 )
            , angle = 0
            , fire = False
        }


accelerate : Float -> Float -> ( Float, Float ) -> ( Float, Float )
accelerate acceleration angle ( x, y ) =
    let
        deg =
            degrees (angle + 90)
    in
        ( acceleration * cos deg + x, acceleration * sin deg + y )
