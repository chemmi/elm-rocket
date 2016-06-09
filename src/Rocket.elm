module Rocket exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Rocket.World exposing (..)
import Keyboard
import Char
import Element exposing (toHtml)
import Time exposing (..)


type alias Model =
    { leftKeyDown : Bool
    , rightKeyDown : Bool
    , forwardKeyDown : Bool
    , rocket : RocketModel
    , str : String
    }


type alias RocketModel =
    { position : ( Float, Float )
    , fire : Bool
    , angle : Float
    , velocity : ( Float, Float )
    , acceleration : Float
    , twist : Float
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
    | NotBound


keyBinding : Char.KeyCode -> Key
keyBinding code =
    if code == Char.toCode 'W' then
        Forward
    else if code == Char.toCode 'A' then
        Left
    else if code == Char.toCode 'D' then
        Right
    else
        NotBound


init =
    ( initModel, Cmd.none )


initModel =
    { leftKeyDown = False
    , rightKeyDown = False
    , forwardKeyDown = False
    , rocket = initRocketModel
    , str = "Show Me Debug"
    }


initRocketModel =
    { acceleration = 0.15
    , position = ( 0, 0 )
    , fire = False
    , angle = 0
    , velocity = ( 0, 0 )
    , twist = 5
    }


subscriptions model =
    Sub.batch
        [ Keyboard.downs (KeyDown << keyBinding)
        , Keyboard.ups (KeyUp << keyBinding)
          -- , every (30 * millisecond) (\_ -> Step)
        , every (30 * millisecond) (\_ -> Step)
        ]


view model =
    let
        rocket =
            model.rocket

        ( a, b ) =
            ( 400, 400 )
    in
        div []
            [ toHtml <| drawScene a b rocket
            , text model.str
            ]


update : Msg -> Model -> Model
update msg model =
    let
        gravity =
            0.1

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

                    NotBound ->
                        model

            Step ->
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
                        }
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



{- Some Helpers -}


accelerate : Float -> Float -> ( Float, Float ) -> ( Float, Float )
accelerate acceleration angle ( x, y ) =
    let
        deg =
            degrees (angle + 90)
    in
        ( acceleration * cos deg + x, acceleration * sin deg + y )
