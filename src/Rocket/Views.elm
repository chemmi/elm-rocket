module Rocket.Views exposing (..)

import Rocket.Types exposing (..)
import Rocket.Scene exposing (..)
import Html exposing (..)
import Element exposing (..)
import Time exposing (inSeconds)
import List
import Debug


viewStartScreen : StartScreenData -> Html a
viewStartScreen data =
    div []
        [ toHtml
            <| layers
                [ container 800 600 middle
                    <| show "StartScreen - Press [SPACE] to choose level"
                ]
        ]


viewWorldChoice : WorldChoiceData -> Html a
viewWorldChoice { worlds } =
    div []
        [ toHtml
            <| layers
                [ drawWorldThumbnail
                    <| case List.head worlds of
                        Just world ->
                            world

                        Nothing ->
                            Debug.crash "No worlds found"
                , container 800 600 middle
                    <| show "<-- [A]    Choose and press [SPACE] to start level    [D] -->"
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
viewPlay data =
    div []
        [ toHtml <| drawScene data
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
