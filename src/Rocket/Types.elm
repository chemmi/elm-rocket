module Rocket.Types exposing (..)


type alias Model =
    { keyDown : KeyDown
    , updateInterval : Float
    , rocket : Rocket
    , world : World
    , str : String
    , gameover : Bool
    }


type alias KeyDown =
    { left : Bool
    , right : Bool
    , forward : Bool
    }


type alias Rocket =
    { position : Point
    , landed : Bool
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
