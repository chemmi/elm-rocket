module Rocket.Intersection exposing (..)

import Collage exposing (Shape, Path)
import List exposing (..)
import Maybe exposing (..)


type alias Point =
    ( Float, Float )


type alias Segment =
    ( Point, Point )


{-| This function takes three points. The output will be '0', if these points lie on a line, '1', if the points are in positive orientation (counter-clockwise), and '-1', if the points are in negative orientation (clockwise).

(Reference: Signed area of a parallelogram)
-}
orientationTest : Point -> Point -> Point -> Int
orientationTest ( px, py ) ( qx, qy ) ( rx, ry ) =
    let
        signedArea =
            (qx - px) * (ry - py) - (rx - px) * (qy - py)
    in
        if signedArea > 0 then
            1
        else if signedArea == 0 then
            0
        else
            -1


pathToSegments : List Point -> List Segment
pathToSegments path =
    case path of
        [] ->
            []

        [ p ] ->
            [ ( p, p ) ]

        [ p, q ] ->
            [ ( p, q ) ]

        p :: q :: rest ->
            ( p, q )
                :: pathToSegments (q :: rest)


intersectionsSegmentPath : Segment -> List Point -> Int
intersectionsSegmentPath l path =
    length (filter (segmentsIntersect l) (pathToSegments path))


segmentsIntersect : Segment -> Segment -> Bool
segmentsIntersect l l' =
    let
        ( lp, lq ) =
            l

        ( l'p, l'q ) =
            l'

        otLinePoint =
            (\( p, q ) r -> orientationTest p q r)
    in
        if
            (otLinePoint l l'p /= otLinePoint l l'q)
                && (otLinePoint l' lp /= otLinePoint l' lq)
        then
            True
        else if
            inSegment lp l'
                || inSegment lq l'
                || inSegment l'p l
                || inSegment l'q l
        then
            True
        else
            False


inSegment : Point -> Segment -> Bool
inSegment p l =
    let
        ( lp, lq ) =
            l

        ( lpx, lpy ) =
            lp

        ( lqx, lqy ) =
            lq

        ( px, py ) =
            p
    in
        if orientationTest p lp lq == 0 then
            let
                factor =
                    if lpx /= lqx then
                        (px - lpx) / (lqx - lpx)
                    else if lpx /= lqx then
                        (py - lpy) / (lqy - lpy)
                    else
                        -1
            in
                if factor >= 0 && factor <= 1 then
                    True
                else
                    False
        else
            False
