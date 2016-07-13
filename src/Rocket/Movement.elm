module Rocket.Movement exposing (..)

import Rocket.Types exposing (..)
import Rocket.Collision exposing (collisionRocketPlatforms)
import Time exposing (..)
import List exposing (map)
import Maybe exposing (oneOf, andThen)


moveRocket : KeyDown -> Float -> Time -> Rocket -> Rocket
moveRocket keyDown gravity diffTime ({ angle, position, fire, velocity, acceleration, twist } as rocket) =
    let
        ( x, y ) =
            position

        ( vx, vy ) =
            velocity

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
                    <| (if keyDown.up then
                            accelerate (acceleration * diffSeconds) angle
                        else
                            identity
                       )
                    <| ( vx, vy )
            , position = ( vx + x, vy + y )
            , fire = keyDown.up
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
tryLanding ({ velocity, position, base, angle } as rocket) { platforms } =
    let
        ( vx, vy ) =
            velocity

        ( rx, ry ) =
            position

        ( b1x, b1y ) =
            fst base

        ( b2x, b2y ) =
            snd base

        roundedAngle =
            round angle % 360

        angleTolerance =
            15

        vxTolerance =
            3

        vyTolerance =
            5

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
                            && (abs vx < vxTolerance)
                            && (abs vy < vyTolerance)
                    then
                        Just platform
                    else
                        Nothing
    in
        andThen (collisionRocketPlatforms rocket platforms) landOnPlatform


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
accelerate acceleration angle ( vx, vy ) =
    let
        deg =
            degrees (angle + 90)
    in
        ( acceleration * cos deg + vx, acceleration * sin deg + vy )
