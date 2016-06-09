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


background : Float -> Float -> Form
background a b =
    filled green <| rect a b


frame : Float -> Float -> Form
frame a b =
    let
        l : LineStyle
        l =
            solid black
    in
        outlined { l | width = 10 } <| rect a b


testScene : Float -> Float -> Element
testScene a b =
    let
        world =
            testWorld
    in
        collage (round a) (round b)
            <| [ background a b
               , world a b
               , rocket False
               , frame a b
               ]



-- A main for testing Worlds
{-

   main =
       let
           ( a, b ) =
               ( 400, 400 )
       in
           toHtml
           <| testScene a b

-}

{- Try Comment -}

rocket : Bool -> Form
rocket fire =
    let
        bodyShape : Shape
        bodyShape =
            polygon [ ( -10, -10 ), ( 0, 15 ), ( 10, -10 ), ( 0, -5 ) ]

        fireShape : Shape
        fireShape =
            polygon [ ( -6, -4 ), ( 0, -12 ), ( 6, -4 ) ]
    in
        group
            <| (if fire then
                    [ filled red fireShape
                    , filled blue bodyShape
                    , outlined defaultLine bodyShape
                      --, filled red fire
                    ]
                else
                    [ filled blue bodyShape
                    , outlined defaultLine bodyShape
                      --, filled red fire
                    ]
               )



--drawScene : Float -> Float -> { position : (Float, Float), angle : Float} ->  Element


drawScene a b r =
    let
        world =
            testWorld
    in
        collage (round a) (round b)
            <| [ background a b
               , world a b
               , move r.position << rotate (degrees r.angle) <| (rocket r.fire)
               , frame a b
               ]
