module Rocket.View exposing (..)

import Rocket.Types exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Color exposing (..)
import List exposing (map, append)


rectShape : AxisParallelRect -> Shape
rectShape rect =
    let
        ( x, y ) =
            rect.topLeft

        h =
            rect.height

        w =
            rect.width
    in
        Collage.polygon
            [ ( x, y )
            , ( x + w, y )
            , ( x + w, y - h )
            , ( x, y - h )
            ]


polyShape : Polygon -> Shape
polyShape poly =
    Collage.polygon poly


platformShape : Platform -> Shape
platformShape platform =
    let
        ( x, y ) =
            platform.center

        hWidth =
            platform.width / 2

        height =
            10
    in
        Collage.polygon
            [ ( x - hWidth, y )
            , ( x + hWidth, y )
            , ( x + hWidth, y - height )
            , ( x - hWidth, y - height )
            ]


worldForm world =
    let
        ( a, b ) =
            world.size

        rects =
            world.axisParallelRects

        polys =
            world.polygons

        color =
            lightCharcoal
    in
        group
            <| append (map (filled color << rectShape) rects)
                (map (filled color << polyShape) polys)


platformsForm platforms =
    let
        line =
            solid black

        markedColor =
            green

        unmarkedColor =
            red

        pColor =
            \p ->
                if p.marked then
                    markedColor
                else
                    unmarkedColor

        pForm =
            \p ->
                let
                    pS =
                        platformShape p
                in
                    group
                        [ filled (pColor p) pS
                        , outlined line pS
                        ]
    in
        group <| map pForm platforms


background : Float -> Float -> Form
background a b =
    filled lightGreen <| rect a b


frameForm : Float -> Float -> Form
frameForm a b =
    let
        line =
            solid black
    in
        outlined { line | width = 2 } <| rect a b


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
               , platformsForm world.platforms
               , frameForm a b
               ]
