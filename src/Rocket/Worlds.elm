module Rocket.Worlds exposing (..)

import Rocket.Types exposing (..)
import List exposing (all)
import Time exposing (Time, second)


initWorld : World
initWorld =
    let
        size =
            ( 800, 600 )
    in
        { size = size
        , gravity = 3
        , totalTime = 60 * second
        , isWin =
            (\ps -> all (\p -> p.marked) ps)
            -- all platforms should be marked
        , rects =
            [ { topLeft = ( -((fst size) / 2), -((snd size) / 3) ), height = (snd size) / 6, width = fst size }
            ]
        , polygons =
            []
        , platforms =
            [ { center = ( 0, -200 ), width = 70, marked = False }
            ]
        , rocketStartPosition = ( 0, 150 )
        }


twoPlatforms : World
twoPlatforms =
    { initWorld
        | rects =
            [ Rect ( -400, -250 ) 800 50
            , Rect ( -400, 0 ) 250 250
            ]
        , polygons =
            [ [ ( -150, 0 ), ( 150, -250 ), ( -150, -250 ) ]
            ]
        , platforms =
            [ Platform ( -250, 0 ) 70 False
            , Platform ( 250, -250 ) 70 False
            ]
        , rocketStartPosition = ( -200, 150 )
    }


outOfSight : World
outOfSight =
    { initWorld
        | size = ( 1600, 600 )
        , rects =
            [ Rect ( -800, -250 ) 1600 50
            ]
        , polygons =
            [ [ ( -500, -250 ), ( 0, 100 ), ( 500, -250 ) ]
            ]
        , platforms =
            [ Platform ( -700, -250 ) 70 False
            , Platform ( 700, -250 ) 70 False
            ]
        , rocketStartPosition = ( -600, 150 )
    }


visitChambers : World
visitChambers =
    { initWorld
        | size = ( 1600, 1200 )
        , rects =
            [ -- Frame
              Rect ( -800, 600 ) 50 1200
            , Rect ( 750, 600 ) 50 1200
            , Rect ( -750, -550 ) 1500 50
            , Rect ( -750, 600 ) 1500 50
              -- middle T
            , Rect ( -25, 550 ) 50 775
            , Rect ( -600, -200 ) 1200 50
              -- Sides
            , Rect ( -750, 200 ) 400 50
            , Rect ( 350, 200 ) 400 50
            ]
        , polygons = []
        , platforms =
            [ Platform ( 0, -550 ) 70 False
            , Platform ( -550, 200) 70 False
            , Platform ( 550, 200) 70 False
            ]
        , rocketStartPosition = ( 0, -540 )
    }
