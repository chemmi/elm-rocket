module Rocket exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.Worlds exposing (..)
import Rocket.Movement exposing (..)
import Rocket.View exposing (..)
import Rocket.Intersection exposing (intersectionsSegmentPath)
import List exposing (..)
import Maybe exposing (andThen)
import Keyboard
import Char exposing (KeyCode, fromCode)
import Element exposing (toHtml)
import Time exposing (..)


initModel =
    let
        world =
            hole
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
    , twist = 3
    , touchesWorld = False
    , base = ( ( -10, -5 ), ( 10, -5 ) )
    , top = ( 0, 20 )
    }


type Msg
    = KeyDownMsg Key
    | KeyUpMsg Key
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
            [ Keyboard.downs (KeyDownMsg << keyBinding)
            , Keyboard.ups (KeyUpMsg << keyBinding)
            , every (model.updateInterval * millisecond) (\_ -> Step)
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
            [ viewValue "Gameover" model.gameover ]
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

            KeyDownMsg _ ->
                { model | keyDown = updateKeyDown keyDown msg }

            KeyUpMsg _ ->
                { model | keyDown = updateKeyDown keyDown msg }

            Step ->
                let
                    touch =
                        touchesWorld rocket world

                    platform =
                        if touch then
                            atPlatform rocket world
                        else
                            rocket.onPlatform

                    landed =
                        rocket.landed
                in
                    if landed then
                        if (model.keyDown).forward then
                            let
                                ( px, py ) =
                                    rocket.position
                            in
                                { model
                                    | rocket =
                                        { rocket
                                            | landed = False
                                            , position = ( px, py + 1 )
                                            , fire = True
                                            , onPlatform = Nothing
                                        }
                                }
                        else
                            model
                    else if touch then
                        case andThen platform (landing model) of
                            Just m ->
                                m

                            Nothing ->
                                { model | gameover = True }
                    else
                        { model
                            | rocket =
                                moveRocket rocket
                                    model.keyDown
                                    world.gravity
                        }

            ShowMe s ->
                { model | str = s }

            _ ->
                model


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


landing : Model -> Platform -> Maybe Model
landing model platform =
    let
        rocket =
            model.rocket

        world =
            model.world

        ( vx, vy ) =
            rocket.velocity

        pos =
            rocket.position

        ( px, py ) =
            pos

        ( b1, b2 ) =
            rocket.base

        ( b1x, b1y ) =
            b1

        ( b2x, b2y ) =
            b2

        roundedAngle =
            round rocket.angle % 360

        angleTolerance =
            10

        vxTolerance =
            1

        vyTolerance =
            2
    in
        if
            ((roundedAngle < angleTolerance)
                || (roundedAngle > 360 - angleTolerance)
            )
                && (b1x + px > platform.from)
                && (b2x + px < platform.to)
                && (abs vx < vxTolerance)
                && (abs vy < vyTolerance)
        then
            Just
                { model
                    | rocket =
                        { rocket
                            | angle = 0
                            , position = ( px, platform.height - b1y )
                            , landed = True
                            , velocity = ( 0, 0 )
                            , fire = False
                            , onPlatform = Just platform
                        }
                    , keyDown = noKeyDown
                    , world =
                        { world
                            | platforms = markPlatform platform world.platforms
                        }
                }
        else
            Nothing


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

        outOfScope =
            let
                ( a, b ) =
                    world.size
            in
                \p ->
                    let
                        ( px, py ) =
                            p
                    in
                        px < -a / 2 || px > a / 2 || py < -b / 2 || py > b / 2
    in
        any checkPoint (map (addPoints pos) [ b1, b2, t ])
            || any outOfScope (map (addPoints pos) [ b1, b2, t ])


addPoints : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addPoints ( px, py ) ( qx, qy ) =
    ( px + qx, py + qy )


atPlatform : Rocket -> World -> Maybe Platform
atPlatform rocket world =
    let
        ( b1, b2 ) =
            rocket.base

        pos =
            rocket.position

        platforms =
            world.platforms

        nearplatform =
            \( px, py ) platform ->
                if platform.from < px && platform.to > px then
                    platform.height - 10 < py && platform.height > py
                else
                    False

        candidates =
            append (filter (nearplatform (addPoints b1 pos)) platforms)
                (filter (nearplatform (addPoints b2 pos)) platforms)
    in
        head candidates
