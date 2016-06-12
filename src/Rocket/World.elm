module Rocket.World exposing (..)

import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Color exposing (..)


type alias RocketData =
    { position : ( Float, Float )
    , angle : Float
    , fire : Bool
    }


testWorld : Float -> Float -> Form
testWorld a b =
    let
        rel =
            1 / 4
    in
        moveY (-b / 2 * (1 - rel))
            <| filled gray
            <| rect a (b * rel)


worldForm world =
    let
        ( a, b ) =
            world.size

        path =
            world.path

        poly =
            polygon (( a / 2, -b / 2 ) :: ( -a / 2, -b / 2 ) :: path)

        l =
            solid black
    in
        group
            [ filled lightCharcoal poly
            , outlined { l | width = 1 } poly
            ]


background : Float -> Float -> Form
background a b =
    filled green <| rect a b


frameForm : Float -> Float -> Form
frameForm a b =
    let
        l : LineStyle
        l =
            solid black
    in
        outlined { l | width = 2 } <| rect a b


rocketForm rocket =
    let
        ( base1, base2 ) =
            rocket.base

        top =
            rocket.top

        fire =
            rocket.fire

        bodyShape : Shape
        bodyShape =
            polygon [ base1, top, base2, ( 0, 0 ) ]

        fireShape : Shape
        fireShape =
            polygon [ ( -6, 1 ), ( 0, -6 ), ( 6, 1 ) ]
    in
        group
            <| (if fire then
                    [ filled red fireShape
                    , filled blue bodyShape
                    , outlined defaultLine bodyShape
                    ]
                else
                    [ filled blue bodyShape
                    , outlined defaultLine bodyShape
                    ]
               )


{-| drawScene : Float -> Float -> { position : (Float, Float), angle : Float} ->  Element
-}
drawScene world rocket =
    let
        ( a, b ) =
            world.size
    in
        collage (round a) (round b)
            <| [ background a b
               , worldForm world
               , move rocket.position
                    << rotate (degrees rocket.angle)
                    <| (rocketForm rocket)
               , frameForm a b
               ]
