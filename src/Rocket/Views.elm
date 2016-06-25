module Rocket.Views exposing (..)

import Rocket.Types exposing (..)
import Rocket.Scene exposing (..)
import Html exposing (..)
import Element exposing (..)
import Time exposing (inSeconds)


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
        [ toHtml
            <| layers
                [ data.background
                , container 800 600 middle (show data.message)
                ]
        ]


viewPlay : PlayData -> Html a
viewPlay ({ rocket, world, timeRemaining, displaySize } as data) =
    div []
        [ toHtml
            <| layers
                [ drawScene data
                , uncurry container
                    displaySize
                    (midRightAt (absolute 10) (absolute 20))
                    <| show
                    <| inSeconds timeRemaining
                ]
          --, viewRocketStatus rocket
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
