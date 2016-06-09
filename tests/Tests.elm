module Main exposing (..)

import Html exposing (..)
import ElmTest exposing (..)


tests : Test
tests =
    suite "Another Test Suite"
        [ suite "A Test Suite"
            [ test "Addition" (assertEqual (3 + 7) 10)
            , test "This test should pass" (assertEqual "a" "a")
            , test "This test should fail" (assert False)
            ]
        ]


main =
    runSuiteHtml tests
