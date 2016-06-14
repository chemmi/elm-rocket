module Rocket.Types exposing (..)


type alias Point =
    ( Float, Float )


type alias Model =
    { leftKeyDown : Bool
    , rightKeyDown : Bool
    , forwardKeyDown : Bool
    , updateInterval : Float
    , rocket : RocketModel
    , world : WorldModel
    , str : String
    , gameover : Bool
    }


type alias RocketModel =
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


type alias WorldModel =
    { path : List Point
    , pointsOutside : ( Point, Point )
    , size : ( Float, Float )
    , gravity : Float
    , platforms : List Platform
    }


type alias Platform =
    { height : Float
    , from : Float
    , to : Float
    , marked : Bool
    }
