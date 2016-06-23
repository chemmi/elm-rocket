module Rocket.Inits exposing (..)

import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Scene exposing (drawScene)
import Element exposing (show)


init =
    Startscreen initStartscreen


initPlay =
    let
        world =
            world2
    in
        { keyDown = noKeyDown
        , updateInterval = 15
        , world = world
        , rocket =
            { initRocket
                | position = world.rocketStartPosition
            }
        , gameover = False
        }


initStartscreen =
    { message = "Startscreen - Press SPACE to start"
    , background = drawScene initPlay.world initPlay.rocket
    }


initGameover =
    { message = "Gameover - Press SPACE to restart"
    , background = show "No Image...."
    }


initWin =
    { message = "Win - Press SPACE to restart"
    , background = show "No Image...."
    }


noKeyDown =
    { left = False
    , right = False
    , forward = False
    }


initRocket =
    { acceleration = initWorld.gravity * 3
    , position = ( 0, 0 )
    , movement = Flying
    , onPlatform = Nothing
    , fire = False
    , angle = 0
    , velocity = ( 0, 0 )
    , twist = 2
    , touchesWorld = False
    , base = ( ( -10, -5 ), ( 10, -5 ) )
    , top = ( 0, 20 )
    }
