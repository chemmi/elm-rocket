module Rocket exposing (..)

import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Movement exposing (..)
import Rocket.View exposing (..)
import Rocket.Collision exposing (collision)
import List exposing (..)
import Keyboard
import Char exposing (KeyCode, fromCode)
import AnimationFrame exposing (..)
import Time exposing (..)


init =
    initStartscreen


initPlay =
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
        , gameover = False
        }


initStartscreen =
    Startscreen "Startscreen"


initGameover =
    Gameover "Gameover"


initWin =
    Win "Win"


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


type Key
    = Left
    | Right
    | Forward
    | Start
    | NotBound


keyBinding : Model -> KeyCode -> Key
keyBinding model code =
    case model of
        Play _ ->
            case fromCode code of
                'W' ->
                    Forward

                'A' ->
                    Left

                'D' ->
                    Right

                _ ->
                    NotBound

        Gameover _ ->
            Start

        Startscreen _ ->
            Start

        Win _ ->
            Start


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ Keyboard.downs (KeyDownMsg << (keyBinding model))
         , Keyboard.ups (KeyUpMsg << (keyBinding model))
         ]
            ++ case model of
                Play _ ->
                    [ diffs Step
                      --, every (playModel.updateInterval * millisecond) (\_ -> Step)
                    ]

                _ ->
                    []
        )


view model =
    case model of
        Play data ->
            viewPlay data

        Startscreen data ->
            viewStartscreen data

        Gameover data ->
            viewGameover data

        Win data ->
            viewWin data


updatePlay : Msg -> PlayData -> PlayData
updatePlay msg data =
    let
        world =
            data.world

        rocket =
            data.rocket

        keyDown =
            data.keyDown
    in
        case msg of
            KeyDownMsg key ->
                { data | keyDown = updateKeyDown keyDown (KeyDownMsg key) }

            KeyUpMsg key ->
                { data | keyDown = updateKeyDown keyDown (KeyUpMsg key) }

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
                        updateLanded data
                    else
                        case tryLanding rocket platforms of
                            Just platform ->
                                { data
                                    | rocket =
                                        landOn platform rocket
                                    , world =
                                        { world
                                            | platforms = markPlatform platform platforms
                                        }
                                }

                            Nothing ->
                                if collision rocket world then
                                    { data | gameover = True }
                                else
                                    { data
                                        | rocket =
                                            moveRocket keyDown
                                                world.gravity
                                                diffTime
                                                rocket
                                    }

            _ ->
                data


update : Msg -> Model -> Model
update msg model =
    case model of
        Play data ->
            Play (updatePlay msg data)

        _ ->
            case msg of
                KeyUpMsg Start ->
                    Play initPlay

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


updateLanded : PlayData -> PlayData
updateLanded data =
    let
        keyDown =
            data.keyDown

        rocket =
            data.rocket
    in
        if keyDown.forward then
            { data | rocket = startRocket rocket }
        else
            data


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
        { init = ( init, Cmd.none )
        , view = view
        , update = \msg playModel -> ( update msg playModel, Cmd.none )
        , subscriptions = subscriptions
        }



{- Some Helpers : -}


addPoints : Point -> Point -> Point
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )
