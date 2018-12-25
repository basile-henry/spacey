port module Main exposing (main)

import Browser
import Browser.Events
import Cmd.Extra exposing (withCmd, withNoCmd)
import Control
import Html exposing (Html, a, button, div, h1, input, p, text)
import Html.Attributes exposing (href, size, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Json.Encode exposing (Value, encode)
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes as Svg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


port parse : String -> Cmd msg


port parseReturn : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ subPort Receive
        , parseReturn Process
        , Browser.Events.onAnimationFrameDelta Tick
        ]



-- MODEL


type alias Model =
    { players : List Player
    , aliens : List Alien
    }


playerWidth : Float
playerWidth =
    8


initPlayers : List Player
initPlayers =
    List.repeat 4
        { x = 50 - playerWidth / 2
        , angle = 0.0
        , left = False
        , right = False
        , shoot = False
        }


initAliens : List Alien
initAliens =
    let
        line n monster width vOffset =
            let
                spacing =
                    width * 1.5
            in
            List.range 0 (n - 1)
                |> List.map
                    (\i ->
                        { life = 2
                        , monster = monster
                        , width = width
                        , points = 20
                        , x = toFloat i * spacing
                        , y = vOffset
                        , minX = toFloat i * spacing
                        , maxX = 100 - toFloat (n - i - 1) * spacing - width
                        , dir = 1
                        }
                    )
    in
    List.concat
        [ line 6 0 8 30
        , line 6 1 8 20
        , line 6 2 8 10
        , line 6 3 8 0
        ]


init : () -> ( Model, Cmd Msg )
init flags =
    ( { players = initPlayers
      , aliens = initAliens
      }
    , parse
        """
        { "module": "WebSocket", "tag": "open", "args": {"key": "foo", "url": "ws://localhost:1234"} }
        """
    )


type alias Player =
    { x : Float
    , angle : Float
    , left : Bool
    , right : Bool
    , shoot : Bool
    }


type alias Alien =
    { x : Float
    , y : Float
    , minX : Float
    , maxX : Float
    , dir : Float
    , life : Int
    , points : Int
    , monster : Int
    , width : Float
    }



-- UPDATE


type Msg
    = Receive Value
    | Process Value
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            tick dt model

        Process value ->
            model
                |> Debug.log ("Process: " ++ encode 0 value)
                |> withCmd (cmdPort value)

        Receive value ->
            case Control.control value of
                Err err ->
                    model
                        |> Debug.log (Json.errorToString err)
                        |> withNoCmd

                Ok ctrl ->
                    let
                        state =
                            case ctrl.state of
                                Control.Down ->
                                    True

                                Control.Up ->
                                    False
                    in
                    { model
                        | players =
                            List.update ctrl.player
                                (\p ->
                                    case ctrl.button of
                                        Control.Left ->
                                            { p | left = state }

                                        Control.Right ->
                                            { p | right = state }

                                        Control.Shoot ->
                                            { p | shoot = state }
                                )
                                model.players
                    }
                        |> withNoCmd


tickPlayer : Float -> Player -> ( Player, Cmd Msg )
tickPlayer dt player =
    let
        dx =
            dt / 30

        da =
            dt / 10

        ( x, angle ) =
            case ( player.left, player.right ) of
                ( True, False ) ->
                    ( max -(playerWidth / 2) (player.x - dx)
                    , max -25 (player.angle - da)
                    )

                ( False, True ) ->
                    ( min (100 - playerWidth / 2) (player.x + dx)
                    , min 25 (player.angle + da)
                    )

                _ ->
                    ( player.x
                    , if player.angle > 0.5 then
                        player.angle - da
                      else if player.angle < -0.5 then
                        player.angle + da
                      else
                        0
                    )
    in
    { player | x = x, angle = angle }
        |> withNoCmd


tickAlien : Float -> Alien -> ( Alien, Cmd Msg )
tickAlien dt alien =
    let
        dx =
            dt / 200

        dy =
            dt / 2000
    in
    (if alien.x > alien.maxX then
        { alien | x = alien.x - dx, dir = -1, y = alien.y + dy }
     else if alien.x < alien.minX then
        { alien | x = alien.x + dx, dir = 1, y = alien.y + dy }
     else
        { alien | x = alien.x + alien.dir * dx, y = alien.y + dy }
    )
        |> withNoCmd


tick : Float -> Model -> ( Model, Cmd Msg )
tick dt model =
    let
        ( players, cmds ) =
            model.players
                |> List.map (tickPlayer dt)
                |> List.unzip

        ( aliens, cmds_ ) =
            model.aliens
                |> List.map (tickAlien dt)
                |> List.unzip
    in
    ( { model | players = players, aliens = aliens }
    , Cmd.batch (cmds ++ cmds_)
    )



-- VIEW


monsterView : Int -> Float -> ( Float, Float ) -> Svg msg
monsterView n width ( x, y ) =
    Svg.image
        [ Svg.xlinkHref ("/res/monster/" ++ String.fromInt n ++ ".png")
        , Svg.x (String.fromFloat x)
        , Svg.y (String.fromFloat y)
        , Svg.width (String.fromFloat width)
        ]
        []


shipView : Int -> Float -> ( Float, Float ) -> Float -> Svg msg
shipView n width ( x, y ) rotation =
    let
        dx =
            String.fromFloat (x + playerWidth / 2)

        dy =
            String.fromFloat (y + playerWidth / 2)
    in
    Svg.image
        [ Svg.xlinkHref ("/res/ship/" ++ String.fromInt n ++ ".png")
        , Svg.transform
            ("rotate(" ++ String.fromFloat rotation ++ " " ++ dx ++ " " ++ dy ++ ")")
        , Svg.x (String.fromFloat x)
        , Svg.y (String.fromFloat y)
        , Svg.width (String.fromFloat width)
        ]
        []


playerView : Int -> Player -> Svg msg
playerView n player =
    shipView n playerWidth ( player.x, 87 ) player.angle


alienView : Alien -> Svg msg
alienView alien =
    monsterView alien.monster alien.width ( alien.x, alien.y )


view : Model -> Html Msg
view model =
    let
        monsters =
            List.map alienView model.aliens

        players =
            List.indexedMap playerView model.players
    in
    Svg.svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 100" ]
        ([ Svg.rect [ Svg.fill "gray", Svg.width "100", Svg.height "100" ] []
         ]
            ++ players
            ++ monsters
        )
