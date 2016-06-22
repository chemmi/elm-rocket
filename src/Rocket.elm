module Rocket exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Movement exposing (..)
import Rocket.View exposing (..)
import Rocket.Collision exposing (collision)
import List exposing (..)
import Keyboard
import Char exposing (KeyCode, fromCode)
import Element exposing (toHtml)
import AnimationFrame exposing (..)
import Time exposing (..)


initModel =
    let
        world =
            world2
    in
        { keyDown = noKeyDown
        , updateInterval = 15
        , world = world
        , rocket =
            { initRocket
                | position = world.rocketStartPosition
            }
        , str = "Show Me Debug"
        , gameover = False
        }


noKeyDown =
    { left = False
    , right = False
    , forward = False
    }


initRocket =
    { acceleration = initWorld.gravity * 3
    , position = ( 0, 0 )
    , landed = False
    , onPlatform = Nothing
    , fire = False
    , angle = 0
    , velocity = ( 0, 0 )
    , twist = 2
    , touchesWorld = False
    , base = ( ( -10, -5 ), ( 10, -5 ) )
    , top = ( 0, 20 )
    }


type Msg
    = KeyDownMsg Key
    | KeyUpMsg Key
    | Step Time
    | NoMsg
      -- ShowMe used for debugging
    | ShowMe String


type Key
    = Left
    | Right
    | Forward
    | Start
    | NotBound


keyBinding : KeyCode -> Key
keyBinding code =
    case fromCode code of
        'W' ->
            Forward

        'A' ->
            Left

        'D' ->
            Right

        _ ->
            NotBound


keyBindingGameover : KeyCode -> Key
keyBindingGameover code =
    case fromCode code of
        'R' ->
            Start

        _ ->
            NotBound


init =
    ( initModel, Cmd.none )


subscriptions model =
    if not model.gameover then
        Sub.batch
            [ Keyboard.downs (KeyDownMsg << keyBinding)
            , Keyboard.ups (KeyUpMsg << keyBinding)
            , diffs Step
              --, every (model.updateInterval * millisecond) (\_ -> Step)
            ]
    else
        Sub.batch
            [ Keyboard.ups (KeyUpMsg << keyBindingGameover)
            , Keyboard.downs (KeyDownMsg << keyBindingGameover)
            ]


view model =
    let
        rocket =
            model.rocket

        world =
            model.world
    in
        div []
            [ toHtml <| drawScene world rocket
            , viewRocketStatus rocket
            , viewStatus model
            ]


viewStatus : Model -> Html a
viewStatus model =
    div []
        [ h3 [] [ text "Model Status" ]
        , table []
            [ viewValue "Gameover" model.gameover
            , viewValue "Show Me" model.str
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
            , viewValue "landed" r.landed
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


update : Msg -> Model -> Model
update msg model =
    let
        world =
            model.world

        rocket =
            model.rocket

        keyDown =
            model.keyDown
    in
        case msg of
            KeyUpMsg Start ->
                initModel

            KeyDownMsg key ->
                { model | keyDown = updateKeyDown keyDown (KeyDownMsg key) }

            KeyUpMsg key ->
                { model | keyDown = updateKeyDown keyDown (KeyUpMsg key) }

            Step diffTime ->
                let
                    coll =
                        collision rocket world

                    platforms =
                        world.platforms

                    landed =
                        rocket.landed
                in
                    if landed then
                        updateLanded model
                    else
                        case tryLanding rocket platforms of
                            Just platform ->
                                { model
                                    | rocket =
                                        landOn platform rocket
                                    , world =
                                        { world
                                            | platforms = markPlatform platform platforms
                                        }
                                }

                            Nothing ->
                                if collision rocket world then
                                    { model | gameover = True }
                                else
                                    { model
                                        | rocket =
                                            moveRocket model.keyDown
                                                world.gravity
                                                diffTime
                                                rocket
                                        , str = toString diffTime
                                    }

            ShowMe s ->
                { model | str = s }

            _ ->
                model


landOn : Platform -> Rocket -> Rocket
landOn platform rocket =
    let
        ( rx, ry ) =
            rocket.position

        ( b1x, b1y ) =
            fst (rocket.base)

        ( cx, cy ) =
            platform.center
    in
        { rocket
            | landed = True
            , position = ( rx, cy - b1y )
            , velocity = ( 0, 0 )
            , angle = 0
            , fire = False
        }


updateKeyDown : KeyDown -> Msg -> KeyDown
updateKeyDown keyDown msg =
    case msg of
        KeyDownMsg key ->
            case key of
                Left ->
                    { keyDown | left = True }

                Right ->
                    { keyDown | right = True }

                Forward ->
                    { keyDown | forward = True }

                _ ->
                    keyDown

        KeyUpMsg key ->
            case key of
                Left ->
                    { keyDown | left = False }

                Right ->
                    { keyDown | right = False }

                Forward ->
                    { keyDown | forward = False }

                _ ->
                    keyDown

        _ ->
            keyDown


updateLanded : Model -> Model
updateLanded model =
    let
        keyDown =
            model.keyDown

        rocket =
            model.rocket
    in
        if keyDown.forward then
            { model | rocket = startRocket rocket }
        else
            model


markPlatform : Platform -> List Platform -> List Platform
markPlatform p ps =
    case ps of
        [] ->
            []

        p' :: ps' ->
            if p == p' then
                { p | marked = True } :: ps'
            else
                p' :: markPlatform p ps'


main =
    program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }



{- Some Helpers : -}


addPoints : Point -> Point -> Point
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )
