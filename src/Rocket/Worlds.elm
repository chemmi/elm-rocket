module Rocket.Worlds exposing (..)

import Rocket.Types exposing (World, Platform)
import List exposing (all)
import Time exposing (Time, second)


initWorld : World
initWorld =
    { size = ( 800, 600 )
    , rects =
        [ { topLeft = ( -400, -150 ), height = 150, width = 800 }
        ]
    , polygons = []
    , gravity = 5
    , platforms = []
    , rocketStartPosition = ( 0, 0 )
    , totalTime = 60 * second
    , isWin = (\ps -> all (\p -> p.marked) ps)
    }


world2 : World
world2 =
    { initWorld
        | rects =
            [ { topLeft = ( -400, -150 ), height = 150, width = 800 }
            , { topLeft = ( 300, 150 ), height = 150, width = 100 }
            ]
        , platforms =
            [ { center = ( 0, -150 ), width = 50, marked = False } ]
    }


world3 : World
world3 =
    { world2
        | size = ( 1600, 600 )
        , polygons = [ [ ( -300, 150 ), ( -200, 200 ), ( -100, 100 ), ( -400, 0 ) ] ]
    }
