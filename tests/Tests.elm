module Main exposing (..)

import ElmTest exposing (..)
import Html exposing (..)
import List exposing (map)


tests : List Test
tests =
    [ 
    ]


main =
    runSuiteHtml (suite "" tests)
