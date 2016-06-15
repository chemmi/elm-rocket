module Rocket.View exposing (..)

import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Color exposing (..)
import List exposing (map)


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


platformsForm platforms =
    let
        line =
            solid black

        markedColor =
            green

        unmarkedColor =
            red

        toForm =
            \p ->
                let
                    form =
                        polygon
                            [ ( p.from, p.height )
                            , ( p.to, p.height )
                            , ( p.to, p.height - 10 )
                            , ( p.from, p.height - 10 )
                            ]

                    color =
                        if p.marked then
                            markedColor
                        else
                            unmarkedColor
                in
                    group
                        [ filled color form
                        , outlined { line | width = 1 } form
                        ]
    in
        group (map toForm platforms)


background : Float -> Float -> Form
background a b =
    filled lightGreen <| rect a b


backgroundImage : Int -> Int -> Element
backgroundImage a b =
    image a b "textures/space.jpg"


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
        layers
            [ backgroundImage (round a) (round b)
            , collage (round a) (round b)
                <| [ worldForm world
                   , move rocket.position
                        << rotate (degrees rocket.angle)
                        <| (rocketForm rocket)
                   , platformsForm world.platforms
                   , frameForm a b
                   ]
            ]
