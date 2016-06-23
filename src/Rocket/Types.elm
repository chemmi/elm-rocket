module Rocket.Types exposing (..)

import Time exposing (Time)
import Element exposing (Element)


type Model
    = Play PlayData
    | Startscreen StartscreenData
    | Gameover GameoverData
    | Win WinData


type Msg
    = KeyDownMsg Key
    | KeyUpMsg Key
    | Step Time
    | TimerTick
    | NoMsg


type Key
    = Left
    | Right
    | Forward
    | Start
    | NotBound


type alias StartscreenData =
    { message : String
    , background : Element
    }


type alias GameoverData =
    { message : String
    , background : Element
    }


type alias WinData =
    { message : String
    , background : Element
    }


type alias PlayData =
    { keyDown : KeyDown
    , timeRemaining : Time
    , rocket : Rocket
    , world : World
    , gameover : Bool
    }


type alias KeyDown =
    { left : Bool
    , right : Bool
    , forward : Bool
    }


type alias Rocket =
    { position : Point
    , movement : Movement
    , onPlatform : Maybe Platform
    , fire : Bool
    , angle : Float
    , velocity : ( Float, Float )
    , acceleration : Float
    , twist : Float
    , touchesWorld : Bool
    , base : ( Point, Point )
    , top : Point
    }


type Movement
    = Landed Platform
    | Landing Platform
    | Flying
    | Colliding


type alias World =
    { size : ( Float, Float )
    , rects : List Rect
    , polygons : List Polygon
    , platforms : List Platform
    , rocketStartPosition : Point
    , gravity : Float
    , totalTime : Time
    , isWin : List Platform -> Bool
    }


type alias Platform =
    { center : Point
    , width : Float
    , marked : Bool
    }


type alias Rect =
    { topLeft : Point
    , height : Float
    , width : Float
    }


type alias Polygon =
    List Point


type alias Point =
    ( Float, Float )
