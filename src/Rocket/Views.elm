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
        StartScreen ->
            viewStartScreen

        InfoScreen data ->
            viewInfoScreen data

        WorldChoiceScreen data ->
            viewWorldChoiceScreen data

        PlayScreen data ->
            viewPlayScreen data

        GameoverScreen data ->
            viewGameoverScreen data

        WinScreen data ->
            viewWinScreen data


viewStartScreen : Html a
viewStartScreen =
    div []
        [ toHtml
            <| layers
                [ showMessageBox
                    ( "Welcome to RocketGame !"
                    , "Press [SPACE] to begin your journey"
                    )
                ]
        ]


viewInfoScreen : InfoData -> Html a
viewInfoScreen data =
    div []
        [ toHtml
            <| layers
                [ showInstructions data
                , showMessageBox
                    ( "Objective"
                    , "Land on all platforms in time !"
                    )
                ]
        ]


showInstructions : InfoData -> Element
showInstructions ({ accelerateAnimation, rotateAnimation, landAnimation } as data) =
    layers
        [ container 800 600 (middleAt (absolute 400) (absolute 200))
            <| Element.flow Element.right
                [ showSmallMessageBox ( "Accelerate", "[W]" )
                , drawPortrait accelerateAnimation
                , Element.spacer 50 1
                , drawPortrait rotateAnimation
                , showSmallMessageBox ( "Rotate", "[A] and [D]" )
                ]
        , container 800 600 (middleAt (absolute 400) (absolute 430))
            <| Element.flow Element.right
                [ showSmallMessageBox ( "Landing", "Not too fast !!" )
                , drawPortrait landAnimation
                , showSmallMessageBox ( "Toggle Music", "[M]" )
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
                , showMessageBox
                    ( "Choose world"
                    , "<-- [A]       [SPACE] : start, [B] : back       [D] -->"
                    )
                ]
        ]


viewGameoverScreen : GameoverData -> Html a
viewGameoverScreen data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , showMessageBox
                    ( "Gameover!"
                    , "[Space] : restart, [B] : choose other world"
                    )
                ]
        ]


viewWinScreen : WinData -> Html a
viewWinScreen data =
    div []
        [ toHtml
            <| layers
                [ data.background
                , showMessageBox
                    ( "Congratulations, you made it !"
                    , "[SPACE], [B]: continue"
                    )
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


showMessageBox : ( String, String ) -> Element
showMessageBox ( caption, message ) =
    let
        default =
            Text.defaultStyle

        height =
            20

        messageStyle =
            { default
                | height = Just height
                , color = black
                , typeface = [ "helvetica" ]
            }

        captionStyle =
            { messageStyle
                | bold = True
                , color = black
            }
    in
        container 800 600 middle
            <| Collage.collage 700 300
            <| [ Collage.moveY (-(toFloat height) * 0.7)
                    <| Collage.alpha 0.8
                    <| Collage.filled grey (Collage.rect 700 (2 * height + 30))
               , Collage.alpha 0.5
                    <| Collage.filled blue (Collage.rect 650 (height + 10))
               , Collage.moveY ((toFloat height) / 8)
                    <| Collage.text
                    <| Text.style captionStyle
                    <| Text.fromString
                    <| caption
               , Collage.moveY ((toFloat height) / 8 - (toFloat height) * 1.5)
                    <| Collage.text
                    <| Text.style messageStyle
                    <| Text.fromString
                    <| message
               ]


showSmallMessageBox : ( String, String ) -> Element
showSmallMessageBox ( caption, message ) =
    let
        default =
            Text.defaultStyle

        height =
            15

        messageStyle =
            { default
                | height = Just height
                , color = black
                , typeface = [ "helvetica" ]
            }

        captionStyle =
            { messageStyle
                | bold = True
                , color = black
            }
    in
        container 130 130 middle
            <| Collage.collage 130 130
            <| [ Collage.moveY (-(toFloat height) * 0.7)
                    <| Collage.alpha 0.8
                    <| Collage.filled grey (Collage.rect 100 (2 * height + 30))
               , Collage.alpha 0.6
                    <| Collage.filled lightBlue (Collage.rect 90 (height + 10))
               , Collage.moveY ((toFloat height) / 8)
                    <| Collage.text
                    <| Text.style captionStyle
                    <| Text.fromString
                    <| caption
               , Collage.moveY ((toFloat height) / 8 - (toFloat height) * 1.7)
                    <| Collage.text
                    <| Text.style messageStyle
                    <| Text.fromString
                    <| message
               ]
