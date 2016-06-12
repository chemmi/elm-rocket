module Rocket exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Rocket.World exposing (..)
import Rocket.Intersection exposing (intersectionsSegmentPath)
import List exposing (any, map)
import Keyboard
import Char exposing (KeyCode, fromCode)
import Element exposing (toHtml)
import Time exposing (..)


type alias Model =
    { leftKeyDown : Bool
    , rightKeyDown : Bool
    , forwardKeyDown : Bool
    , updateInterval : Float
    , rocket : RocketModel
    , world : WorldModel
    , str : String
    , gameover : Bool
    }


initModel =
    { leftKeyDown = False
    , rightKeyDown = False
    , forwardKeyDown = False
    , updateInterval = 20
    , rocket = initRocketModel
    , world = hole
    , str = "Show Me Debug"
    , gameover = False
    }


type alias RocketModel =
    { position : ( Float, Float )
    , landed : Bool
    , fire : Bool
    , angle : Float
    , velocity : ( Float, Float )
    , acceleration : Float
    , twist : Float
    , touchesWorld : Bool
    , base : ( ( Float, Float ), ( Float, Float ) )
    , top : ( Float, Float )
    }


initRocketModel =
    { acceleration = initWorldModel.gravity * 3
    , position = ( 0, 0 )
    , landed = False
    , fire = False
    , angle = 0
    , velocity = ( 0, 0 )
    , twist = 5
    , touchesWorld = False
    , base = ( ( -10, -5 ), ( 10, -5 ) )
    , top = ( 0, 20 )
    }


type alias WorldModel =
    { path : List ( Float, Float )
    , pointsOutside : ( ( Float, Float ), ( Float, Float ) )
    , size : ( Float, Float )
    , gravity : Float
    }


initWorldModel =
    { size = ( 800, 600 )
    , pointsOutside = ( ( 0, 300 ), ( -200, 200 ) )
    , path = [ ( -400, -150 ), ( 400, -150 ) ]
    , gravity = 0.05
    }


world1 =
    { initWorldModel
        | path =
            [ ( -400, 150 )
            , ( -200, -200 )
            , ( -100, -200 )
            , ( 0, -100 )
            , ( 100, -100 )
            , ( 400, -200 )
            ]
    }


hole =
    { initWorldModel
        | path =
            [ ( -400, 200 )
            , ( -100, 200 )
            , ( -100, -150 )
            , ( -120, -150 )
            , ( -120, 150 )
            , ( -380, 150 )
            , ( -380, -280 )
            , ( 400, -280 )
            ]
    }


type Msg
    = KeyDown Key
    | KeyUp Key
    | Step
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
            [ Keyboard.downs (KeyDown << keyBinding)
            , Keyboard.ups (KeyUp << keyBinding)
            , every (model.updateInterval * millisecond) (\_ -> Step)
            ]
    else
        Sub.batch
            [ Keyboard.ups (KeyUp << keyBindingGameover)
            , Keyboard.downs (KeyDown << keyBindingGameover)
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
            ]


viewRocketStatus : RocketModel -> Html a
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

        gravity =
            world.gravity

        rocket =
            model.rocket

        ( x, y ) =
            rocket.position

        angle =
            rocket.angle

        fire =
            rocket.fire

        ( vx, vy ) =
            rocket.velocity

        acceleration =
            rocket.acceleration

        twist =
            rocket.twist
    in
        case msg of
            KeyDown key ->
                case key of
                    Left ->
                        { model | leftKeyDown = True }

                    Right ->
                        { model | rightKeyDown = True }

                    Forward ->
                        { model | forwardKeyDown = True }

                    Start ->
                        model

                    NotBound ->
                        model

            KeyUp key ->
                case key of
                    Left ->
                        { model | leftKeyDown = False }

                    Right ->
                        { model | rightKeyDown = False }

                    Forward ->
                        { model | forwardKeyDown = False }

                    Start ->
                        initModel

                    NotBound ->
                        model

            Step ->
                let
                    touch =
                        touchesWorld rocket world
                in
                    { model
                        | rocket =
                            { rocket
                                | angle =
                                    if model.leftKeyDown then
                                        angle + twist
                                    else if model.rightKeyDown then
                                        angle - twist
                                    else
                                        angle
                                , velocity =
                                    accelerate gravity 180
                                        <| (if model.forwardKeyDown then
                                                accelerate acceleration angle
                                            else
                                                identity
                                           )
                                        <| ( vx, vy )
                                , position = ( vx + x, vy + y )
                                , fire = model.forwardKeyDown
                                , touchesWorld = touch
                            }
                        , gameover = touch
                    }

            ShowMe s ->
                { model | str = s }

            _ ->
                model


main =
    program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }



{- Some Helpers : -}


accelerate : Float -> Float -> ( Float, Float ) -> ( Float, Float )
accelerate acceleration angle ( x, y ) =
    let
        deg =
            degrees (angle + 90)
    in
        ( acceleration * cos deg + x, acceleration * sin deg + y )


touchesWorld rocket world =
    let
        ( p1, p2 ) =
            world.pointsOutside

        ( b1, b2 ) =
            rocket.base

        t =
            rocket.top

        pos =
            rocket.position

        path =
            world.path

        isOdd =
            (\x ->
                if x % 2 == 1 then
                    True
                else
                    False
            )

        checkPoint =
            \p ->
                (isOdd <| intersectionsSegmentPath ( p1, p ) path)
                    && (isOdd <| intersectionsSegmentPath ( p2, p ) path)
    in
        any checkPoint (map (addPoints pos) [ b1, b2, t ])


addPoints : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )
