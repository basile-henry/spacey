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
    }


playerWidth : Float
playerWidth =
    8


init : () -> ( Model, Cmd Msg )
init flags =
    ( { players =
            List.repeat 4
                { x = 50 - playerWidth / 2
                , angle = 0.0
                , left = False
                , right = False
                , shoot = False
                }
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
                    , if player.angle > 0 then
                        player.angle - da
                      else if player.angle < 0 then
                        player.angle + da
                      else
                        player.angle
                    )
    in
    { player | x = x, angle = angle }
        |> withNoCmd


tick : Float -> Model -> ( Model, Cmd Msg )
tick dt model =
    let
        ( players, cmds ) =
            model.players
                |> List.map (tickPlayer dt)
                |> List.unzip
    in
    ( { model | players = players }
    , Cmd.batch cmds
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


view : Model -> Html Msg
view model =
    let
        players =
            List.indexedMap playerView model.players
    in
    Svg.svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 100" ]
        ([ Svg.rect [ Svg.fill "gray", Svg.width "100", Svg.height "100" ] []
         ]
            ++ players
        )
