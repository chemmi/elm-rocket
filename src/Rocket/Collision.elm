module Rocket.Collision exposing (..)

import Rocket.Types exposing (..)
import List exposing (any)

collision : Rocket -> World -> Bool
collision rocket world =
    collisionRocketRects rocket world.axisParallelRects

collisionRocketRects : Rocket -> List AxisParallelRect -> Bool
collisionRocketRects rocket rects =
    let
        ( tx, ty ) =
            addPoints rocket.position rocket.top

        ( b1x, b1y ) =
            addPoints rocket.position (fst rocket.base)

        ( b2x, b2y ) =
            addPoints rocket.position (snd rocket.base)

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
                    ( x, y ) =
                        r.topLeft

                    h =
                        r.height

                    w =
                        r.width
                in
                    not
                        ((x > xmax)
                            || (y < ymin)
                            || (x + w < xmin)
                            || (y - h > ymax)
                        )
    in
        any collRect rects


addPoints : Point -> Point -> Point
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )
