module Rocket.Types exposing (..)


type Model
    = Play PlayData
    | Startscreen StartscreenData
    | Gameover GameoverData
    | Win WinData


type alias StartscreenData =
    String


type alias GameoverData =
    String


type alias WinData =
    String


type alias PlayData =
    { keyDown : KeyDown
    , updateInterval : Float
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
    , axisParallelRects : List AxisParallelRect
    , polygons : List Polygon
    , platforms : List Platform
    , rocketStartPosition : Point
    , gravity : Float
    }


type alias Platform =
    { center : Point
    , width : Float
    , marked : Bool
    }


type alias AxisParallelRect =
    { topLeft : Point
    , height : Float
    , width : Float
    }


type alias Polygon =
    List Point


type alias Point =
    ( Float, Float )
