module Rocket.Collision exposing (..)

import Rocket.Types exposing (..)
import List exposing (any)


collision : Rocket -> World -> Bool
collision rocket world =
    collisionRocketRects rocket world.axisParallelRects


collisionRocketRects : Rocket -> List AxisParallelRect -> Bool
collisionRocketRects rocket rects =
    let
        angle =
            rocket.angle

        rotate =
            \( x, y ) ->
                let
                    cosA =
                        cos <| degrees angle

                    sinA =
                        sin <| degrees angle
                in
                    ( cosA * x - sinA * y, sinA * x + cosA * y )

        ( tx, ty ) =
            addPoints rocket.position
                <| rotate rocket.top

        ( b1x, b1y ) =
            addPoints rocket.position
                <| rotate (fst rocket.base)

        ( b2x, b2y ) =
            addPoints rocket.position
                <| rotate (snd rocket.base)

        {- rectangle around rocket (rocket rect) -}
        ymax =
            max ty (max b1y b2y)

        ymin =
            min ty (min b1y b2y)

        xmax =
            max tx (max b1x b2x)

        xmin =
            min tx (min b1x b2x)

        collRect =
            \r ->
                let
                    ( rx, ry ) =
                        r.topLeft

                    h =
                        r.height

                    w =
                        r.width
                in
                    {- rocket rect collides with rect r
                       == not (rocket rect outside of rect r)
                    -}
                    (not
                        ((rx > xmax)
                            || (ry < ymin)
                            || (rx + w < xmin)
                            || (ry - h > ymax)
                        )
                    )
                        && {- TODO: Add fine collision -} True
    in
        any collRect rects


addPoints : Point -> Point -> Point
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )
