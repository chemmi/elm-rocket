module Rocket.Scene exposing (..)

import Rocket.Types exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Color exposing (..)
import List exposing (map)


rectShape : Rect -> Shape
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


worldForm : World -> Form
worldForm { rects, polygons, platforms } =
    let
        color =
            lightCharcoal
    in
        group
            (platformsForm platforms
                :: map (filled color << rectShape) rects
                ++ map (filled color << polyShape) polygons
            )


platformsForm : List Platform -> Form
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


backgroundForm : ( Float, Float ) -> Form
backgroundForm =
    filled lightGreen << uncurry rect


frameForm : ( Float, Float ) -> Form
frameForm =
    let
        line =
            solid black
    in
        outlined { line | width = 2 } << uncurry rect


rocketForm : Rocket -> Form
rocketForm { position, angle, fire, top, base } =
    let
        ( base1, base2 ) =
            base

        bodyShape : Shape
        bodyShape =
            polygon [ base1, top, base2, ( 0, 0 ) ]

        fireShape : Shape
        fireShape =
            polygon [ ( -6, 1 ), ( 0, -6 ), ( 6, 1 ) ]
    in
        move position
            << rotate (degrees angle)
            <| group
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


drawScene : World -> Rocket -> Element
drawScene ({ size } as world) rocket =
    let
        ( a, b ) =
            size
    in
        collage (round a) (round b)
            <| [ backgroundForm size
               , worldForm world
               , rocketForm rocket
               , frameForm size
               ]
