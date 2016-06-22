module Rocket.Views exposing (..)

import Rocket.Types exposing (..)
import Rocket.Scene exposing (..)
import Html exposing (..)
import Element exposing (..)


viewStartscreen : StartscreenData -> Html a
viewStartscreen data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , container 800 600 middle (show data.message)
                ]
        ]


viewGameover : GameoverData -> Html a
viewGameover data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , container 800 600 middle (show data.message)
                ]
        ]


viewWin : WinData -> Html a
viewWin data =
    div []
        [ text data ]


viewPlay : PlayData -> Html a
viewPlay data =
    let
        rocket =
            data.rocket

        world =
            data.world
    in
        div []
            [ toHtml <| drawScene world rocket
            , viewRocketStatus rocket
            , viewPlayStatus data
            ]


viewPlayStatus : PlayData -> Html a
viewPlayStatus data =
    div []
        [ h3 [] [ text "Play Status" ]
        , table []
            [ viewValue "Gameover" data.gameover
            ]
        ]


viewRocketStatus : Rocket -> Html a
viewRocketStatus r =
    div []
        [ h3 [] [ text "Rocket Status" ]
        , table []
            [ viewValue "Position"
                <| let
                    ( x, y ) =
                        r.position
                   in
                    ( round x, round y )
            , viewValue "Angle" <| round r.angle
            , viewValue "Velocity"
                <| let
                    ( vx, vy ) =
                        r.velocity
                   in
                    ( round vx, round vy )
            , viewValue "Fire" r.fire
            , viewValue "Acceleration" r.acceleration
            , viewValue "Twist" r.twist
            , viewValue "touches World" r.touchesWorld
            , viewValue "Movement" r.movement
            , viewValue "on Platform" r.onPlatform
            ]
        ]


viewValue : String -> a -> Html b
viewValue name value =
    tr []
        [ td [] [ text name ]
        , td []
            [ text <| toString value
            ]
        ]
