module Rocket.Views exposing (..)

import Rocket.Types exposing (..)
import Rocket.Scene exposing (..)
import Html exposing (..)
import Element exposing (..)
import Time exposing (inSeconds)
import List
import Collage
import Text
import Color exposing (..)
import Debug


viewScreen : Screen -> Html a
viewScreen screen =
    case screen of
        PlayScreen data ->
            viewPlayScreen data

        StartScreen ->
            viewStartScreen

        WorldChoiceScreen data ->
            viewWorldChoiceScreen data

        GameoverScreen data ->
            viewGameoverScreen data

        WinScreen data ->
            viewWinScreen data


viewStartScreen : Html a
viewStartScreen =
    div []
        [ toHtml
            <| layers
                [ showMessageBox "StartScreen - Press [SPACE] to choose level"
                ]
        ]


viewWorldChoiceScreen : WorldChoiceData -> Html a
viewWorldChoiceScreen { worldChoice } =
    div []
        [ toHtml
            <| layers
                [ drawWorldThumbnail
                    <| case List.head worldChoice of
                        Just world ->
                            world

                        Nothing ->
                            Debug.crash "No worlds found"
                , showMessageBox "<-- [A]    Choose and press [SPACE] to start level    [D] -->"
                ]
        ]


viewGameoverScreen : GameoverData -> Html a
viewGameoverScreen data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , showMessageBox data.message
                ]
        ]


viewWinScreen : WinData -> Html a
viewWinScreen data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , showMessageBox data.message
                ]
        ]


viewPlayScreen : PlayData -> Html a
viewPlayScreen data =
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


showMessageBox : String -> Element
showMessageBox message =
    container 800 600 middle
        <| Collage.collage 800 300
        <| [ Collage.alpha 0.8
                <| Collage.moveY -3
                <| Collage.filled yellow (Collage.rect 700 40)
           , Collage.text
                <| Text.height 20
                <| Text.color red
                <| Text.typeface [ "Helvetica" ]
                <| Text.fromString
                <| message
           ]
