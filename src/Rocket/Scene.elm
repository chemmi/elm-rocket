module Rocket.Scene exposing (..)

import Rocket.Types exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import List exposing (map)
import Time exposing (Time, inSeconds)
import Text


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
            (map (filled color << rectShape) rects
                ++ map (filled color << polyShape) polygons
                ++ [ platformsForm platforms ]
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
backgroundForm ( a, b ) =
    let
        a' =
            round a

        b' =
            round b
    in
        toForm <| fittedImage a' b' "img/space.jpg"


frameForm : ( Float, Float ) -> Form
frameForm =
    let
        line =
            solid white
    in
        outlined { line | width = 6 } << uncurry rect


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


countdownForm : Time -> Form
countdownForm t =
    let
        default =
            Text.defaultStyle

        style =
            { default
                | height = Just 20
                , color = red
                , typeface = [ "helvetica" ]
            }
    in
        Collage.text
            <| Text.style style
            <| Text.fromString
            <| toString
            <| inSeconds t


drawScene : PlayData -> Element
drawScene { world, rocket, displaySize, displayPosition, timeRemaining } =
    let
        offset =
            ( -(toFloat <| fst displayPosition), -(toFloat <| snd displayPosition) )

        timerPos =
            ( toFloat <| fst displaySize // 2 - 20, toFloat <| snd displaySize // 2 - 15 )
    in
        uncurry collage displaySize
            <| [ move offset
                    <| group
                        [ backgroundForm world.size
                        , worldForm world
                        , rocketForm rocket
                        , frameForm world.size
                        ]
               , move timerPos
                    <| countdownForm timeRemaining
               ]


drawWorldThumbnail : World -> Element
drawWorldThumbnail world =
    let
        displaySize =
            ( 800, 600 )

        -- has to be here to prevent circle...
        initRocket =
            { acceleration = 0
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

        rocket =
            { initRocket | position = world.rocketStartPosition }

        factorx =
            (toFloat <| fst displaySize) / (fst world.size)

        factory =
            (toFloat <| snd displaySize) / (snd world.size)

        factor =
            min factorx factory
    in
        uncurry collage displaySize
            <| [ scale factor
                    <| group
                        [ backgroundForm world.size
                        , worldForm world
                        , rocketForm rocket
                        , frameForm world.size
                        ]
               ]


drawPortrait : PortraitData -> Element
drawPortrait { rocket, platform } =
    collage 130 130
        <| [ backgroundForm ( 110, 110 ), rocketForm rocket ]
        ++ (case platform of
                Just p ->
                    [ moveY (snd p.center - (55 + snd p.center) / 2)
                        <| filled lightCharcoal (rect 110 (55 + snd p.center))
                    , platformsForm [ p ]
                    ]

                Nothing ->
                    []
           )
        ++ [ outlined (solid white) (rect 110 110) ]
