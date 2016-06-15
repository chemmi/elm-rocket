module Rocket.Worlds exposing (..)

import Rocket.Types exposing (World, Platform)


initWorld =
    { size = ( 800, 600 )
    , pointsOutside = ( ( 0, 600 ), ( -200, 200 ) )
    , path = [ ( -400, -150 ), ( 400, -150 ) ]
    , gravity = 5
    , platforms = []
    , rocketStartPosition = ( 0, 0 )
    }


world1 =
    { initWorld
        | path =
            [ ( -400, 150 )
            , ( -200, -200 )
            , ( -100, -200 )
            , ( 0, -100 )
            , ( 100, -100 )
            , ( 400, -200 )
            ]
    }


hole =
    { initWorld
        | path =
            [ ( -400, 200 )
            , ( -100, 200 )
            , ( -100, -150 )
            , ( -120, -150 )
            , ( -120, 150 )
            , ( -380, 150 )
            , ( -380, -280 )
            , ( 400, -280 )
            ]
        , rocketStartPosition = ( 250, -200 )
        , platforms =
            [ { height = -280
              , from = 200
              , to = 300
              , marked = False
              }
            ]
    }
