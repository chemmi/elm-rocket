module Rocket.Worlds exposing (..)

import Rocket.Types exposing (World, Platform)
import List exposing (all)


initWorld : World
initWorld =
    { size = ( 800, 600 )
    , axisParallelRects =
        [ { topLeft = ( -400, -150 ), height = 150, width = 800 }
        ]
    , polygons = []
    , gravity = 5
    , platforms = []
    , rocketStartPosition = ( 0, 0 )
    , isWin = (\ps -> all (\p -> p.marked) ps)
    }


world2 : World
world2 =
    { initWorld
        | axisParallelRects =
            [ { topLeft = ( -400, -150 ), height = 150, width = 800 }
            , { topLeft = ( 300, 150 ), height = 150, width = 100 }
            ]
        , platforms =
            [ { center = ( 0, -140 ), width = 50, marked = False } ]
    }
