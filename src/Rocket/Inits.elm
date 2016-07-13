module Rocket.Inits exposing (..)

import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Scene exposing (drawScene, drawWorldThumbnail)
import Element exposing (empty)


init : Model
init =
    StartScreen


initPlay : PlayData
initPlay =
    let
        world =
            visitChambers
    in
        { keyDown = noKeyDown
        , timeRemaining = world.totalTime
        , displaySize = ( 800, 600 )
        , displayPosition = ( 0, 0 )
        , world = world
        , rocket =
            { initRocket
                | position = world.rocketStartPosition
            }
        , gameover = False
        }


initWorldChoice : WorldChoiceData
initWorldChoice =
    { worlds = allWorlds }


initGameover : GameoverData
initGameover =
    { message = "Gameover - Press [SPACE] to continue"
    , background = empty
    }


initWin : WinData
initWin =
    { message = "YEAH! WIN! - Press [SPACE] to continue"
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
    { acceleration = initWorld.gravity * 2
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
