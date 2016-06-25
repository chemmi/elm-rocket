module Rocket.Collision exposing (..)

import Rocket.Types exposing (..)
import List exposing (any, all)


collision : Rocket -> World -> Bool
collision rocket { rects, size } =
    (collisionRocketRects rocket rects)
        || (collisionRocketBorder rocket size)


collisionRocketRects : Rocket -> List Rect -> Bool
collisionRocketRects { angle, position, top, base } rects =
    let
        -- maybe add lazy here
        cosAngle =
            cos <| degrees angle

        sinAngle =
            sin <| degrees angle

        rotate =
            \( x, y ) ->
                ( cosAngle * x - sinAngle * y
                , sinAngle * x + cosAngle * y
                )

        (( tx, ty ) as t) =
            addPoints position
                <| rotate top

        (( b1x, b1y ) as b1) =
            addPoints position
                <| rotate (fst base)

        (( b2x, b2y ) as b2) =
            addPoints position
                <| rotate (snd base)

        {- rectangle around rocket (rocket rect) -}
        ymax =
            max ty (max b1y b2y)

        ymin =
            min ty (min b1y b2y)

        xmax =
            max tx (max b1x b2x)

        xmin =
            min tx (min b1x b2x)

        {- coarse collision test -}
        collRectCoarse =
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
                    not
                        ((rx > xmax)
                            || (ry < ymin)
                            || (rx + w < xmin)
                            || (ry - h > ymax)
                        )

        {- fine collision test (uses collRectCoarse) -}
        collRectFine =
            \r ->
                let
                    ( rx, ry ) =
                        r.topLeft

                    h =
                        r.height

                    w =
                        r.width

                    edges =
                        [ ( rx, ry )
                        , ( rx + w, ry )
                        , ( rx + w, ry - h )
                        , ( rx, ry - h )
                        ]

                    ot =
                        orientationTest
                in
                    collRectCoarse r
                        && not
                            (all (ot t b2) edges
                                || all (ot b1 t) edges
                                || all (ot b2 b1) edges
                            )

        collRect =
            \r ->
                collRectFine r
    in
        any collRect rects


collisionRocketBorder : Rocket -> ( Float, Float ) -> Bool
collisionRocketBorder { angle, position, top, base } ( w, h ) =
    let
        -- maybe add lazy here
        cosAngle =
            cos <| degrees angle

        sinAngle =
            sin <| degrees angle

        rotate =
            \( x, y ) ->
                ( cosAngle * x - sinAngle * y
                , sinAngle * x + cosAngle * y
                )

        t =
            addPoints position
                <| rotate top

        b1 =
            addPoints position
                <| rotate (fst base)

        b2 =
            addPoints position
                <| rotate (snd base)

        inBorder =
            \( x, y ) ->
                (-w / 2 < x)
                    && (x < w / 2)
                    && (-h / 2 < y)
                    && (y < h / 2)
    in
        any (not << inBorder) [ t, b1, b2 ]


addPoints : Point -> Point -> Point
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )


{-| This function takes three points. The output will be True, if and only if these points are in positive orientation (counter-clockwise).

(Reference: Signed area of a parallelogram)
-}
orientationTest : Point -> Point -> Point -> Bool
orientationTest ( px, py ) ( qx, qy ) ( rx, ry ) =
    let
        signedArea =
            (qx - px) * (ry - py) - (rx - px) * (qy - py)
    in
        signedArea > 0
