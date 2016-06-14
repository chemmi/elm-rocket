module Rocket.Movement exposing (..)

import Rocket.Types exposing (..)


moveRocket : Rocket -> KeyDown -> Float -> Rocket
moveRocket rocket keyDown gravity =
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
                accelerate gravity 180
                    <| (if keyDown.forward then
                            accelerate acceleration angle
                        else
                            identity
                       )
                    <| ( vx, vy )
            , position = ( vx + x, vy + y )
            , fire = keyDown.forward
        }


accelerate : Float -> Float -> ( Float, Float ) -> ( Float, Float )
accelerate acceleration angle ( x, y ) =
    let
        deg =
            degrees (angle + 90)
    in
        ( acceleration * cos deg + x, acceleration * sin deg + y )
