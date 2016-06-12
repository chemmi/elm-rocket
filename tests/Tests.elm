module Main exposing (..)

import ElmTest exposing (..)
import Html exposing (..)
import List exposing (map)
import Rocket.Intersection
    exposing
        ( orientationTest
        , segmentsIntersect
        , inSegment
        , pathToSegments
        , intersectionsSegmentPath
        )


testOrientationTest : Test
testOrientationTest =
    suite "Tests orientationTest"
        [ test "Positive orientation"
            <| assert
            <| orientationTest ( 2, 2 ) ( 2, 1 ) ( 3, 1 )
            > 0
        , test "Negative orientation"
            <| assert
            <| orientationTest ( 4, 5 ) ( 0, 6 ) ( 1, 7 )
            < 0
        , test "On a line"
            <| assert
            <| orientationTest ( 1, 2 ) ( 2, 4 ) ( 4, 8 )
            == 0
        ]


testSegmentsIntersect : Test
testSegmentsIntersect =
    suite "Tests segmentIntersect"
        [ test "Intersection"
            <| assert
            <| segmentsIntersect
                ( ( -2, 2 )
                , ( 2, -2 )
                )
                ( ( -1, 0 )
                , ( 3, 3 )
                )
        , test "No Intersection"
            <| assert
            << not
            <| segmentsIntersect
                ( ( -2, 4 )
                , ( 2, 4 )
                )
                ( ( 3, 5 )
                , ( 3, 1 )
                )
        , test "On a line no Intersection"
            <| assert
            << not
            <| segmentsIntersect
                ( ( 1, 1 )
                , ( 3, 1 )
                )
                ( ( 4, 1 )
                , ( 6, 1 )
                )
        , test "On a line with Intersection"
            <| assert
            <| segmentsIntersect
                ( ( 1, 1 )
                , ( 6, 1 )
                )
                ( ( 4, 1 )
                , ( 3, 1 )
                )
        ]


testInSegment : Test
testInSegment =
    suite "Tests inSegment"
        [ test "Is not on line"
            <| assert
            << not
            <| inSegment ( 1, 1 ) ( ( 2, 2 ), ( -1, 0 ) )
        , test "Is not in Segment"
            <| assert
            << not
            <| inSegment ( 0, 0 ) ( ( 1, 2 ), ( 3, 6 ) )
        , test "Is in Segment"
            <| assert
            <| inSegment ( 2, 4 ) ( ( 1, 2 ), ( 3, 6 ) )
        ]


testPathToSegments : Test
testPathToSegments =
    suite "Tests pathToSegments"
        [ defaultTest
            <| assertEqual
                (pathToSegments
                    [ ( 1, 1 )
                    , ( 2, 3 )
                    , ( 4, 5 )
                    , ( 8, 9 )
                    ]
                )
                [ ( ( 1, 1 ), ( 2, 3 ) )
                , ( ( 2, 3 ), ( 4, 5 ) )
                , ( ( 4, 5 ), ( 8, 9 ) )
                ]
        ]


testIntersectionSegmentPath : Test
testIntersectionSegmentPath =
    suite "Tests intersectionSegmentPath"
        [ test "One intersection"
            <| assert
            <| intersectionsSegmentPath ( ( 0, 1 ), ( 0, -1 ) ) [ ( 1, 0 ), ( -1, 0 ) ]
            == 1
        , test "Two Intersections"
            <| assert
            <| intersectionsSegmentPath ( ( 0, 1 ), ( 0, -1 ) )
                [ ( 1, 0 ), ( -1, 0 ), ( 0.5, 0.5 ) ]
            == 2
        ]


tests : List Test
tests =
    [ testOrientationTest
    , testSegmentsIntersect
    , testInSegment
    , testPathToSegments
    , testIntersectionSegmentPath
    ]


main =
    runSuiteHtml (suite "" tests)
