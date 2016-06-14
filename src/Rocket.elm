module Rocket exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Rocket.Types exposing (..)
import Rocket.View exposing (..)
import Rocket.Intersection exposing (intersectionsSegmentPath)
import List exposing (..)
import Maybe exposing (andThen)
import Keyboard
import Char exposing (KeyCode, fromCode)
import Element exposing (toHtml)
import Time exposing (..)


initModel =
    { leftKeyDown = False
    , rightKeyDown = False
    , forwardKeyDown = False
    , updateInterval = 15
    , rocket = initRocketModel
    , world = hole
    , str = "Show Me Debug"
    , gameover = False
    }


initRocketModel =
    { acceleration = initWorldModel.gravity * 3
    , position = ( 250, -200 )
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


initWorldModel =
    { size = ( 800, 600 )
    , pointsOutside = ( ( 0, 600 ), ( -200, 200 ) )
    , path = [ ( -400, -150 ), ( 400, -150 ) ]
    , gravity = 0.05
    , platforms = []
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
        , platforms =
            [ { height = -280
              , from = 200
              , to = 300
              , marked = False
              }
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
            , viewStatus model
            ]


viewStatus : Model -> Html a
viewStatus model =
    div []
        [ h3 [] [ text "Model Status" ]
        , table []
            [ viewValue "Gameover" model.gameover ]
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

                    platform =
                        if touch then
                            atPlatform rocket world
                        else
                            rocket.onPlatform

                    landed =
                        rocket.landed
                in
                    if landed then
                        if model.forwardKeyDown then
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
                        updateRocket model

            ShowMe s ->
                { model | str = s }

            _ ->
                model


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
                    , leftKeyDown = False
                    , rightKeyDown = False
                    , forwardKeyDown = False
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


updateRocket : Model -> Model
updateRocket model =
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


atPlatform : RocketModel -> WorldModel -> Maybe Platform
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
