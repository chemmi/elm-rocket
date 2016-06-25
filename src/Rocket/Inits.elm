module Rocket.Inits exposing (..)

import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Scene exposing (drawScene)
import Element exposing (empty)


init : Model
init =
    Startscreen initStartscreen


initPlay : PlayData
initPlay =
    let
        world =
            world3
    in
        { keyDown = noKeyDown
        , timeRemaining = world.totalTime
        , world = world
        , rocket =
            { initRocket
                | position = world.rocketStartPosition
            }
        , gameover = False
        }


initStartscreen : StartscreenData
initStartscreen =
    { message = "Startscreen - Press SPACE to start"
    , background = drawScene initPlay.world initPlay.rocket
    }


initGameover : GameoverData
initGameover =
    { message = "Gameover - Press SPACE to restart"
    , background = empty
    }


initWin : WinData
initWin =
    { message = "Win - Press SPACE to restart"
    , background = empty
    }


noKeyDown : KeyDown
noKeyDown =
    { left = False
    , right = False
    , forward = False
    }


initRocket : Rocket
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
