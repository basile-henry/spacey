port module Main exposing (main)

import Browser
import Cmd.Extra exposing (withCmd, withNoCmd)
import Html exposing (Html, a, button, div, h1, input, p, text)
import Html.Attributes exposing (href, size, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Json.Encode exposing (Value, encode)
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
        ]



-- MODEL


type alias Model =
    { x : Int
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { x = 40
      }
    , parse
        """
        { "module": "WebSocket", "tag": "open", "args": {"key": "foo", "url": "ws://localhost:1234"} }
        """
    )


type Button
    = Left
    | Shoot
    | Right


type alias Control =
    { player : Int
    , button : Button
    }


control : Value -> Result Json.Error Control
control value =
    let
        buttonDecoder =
            Json.string
                |> Json.andThen
                    (\btn ->
                        case btn of
                            "Left" ->
                                Json.succeed Left

                            "Shoot" ->
                                Json.succeed Shoot

                            "Right" ->
                                Json.succeed Right

                            _ ->
                                Json.fail btn
                    )

        controlDecoder =
            Json.map2
                Control
                (Json.field "index" Json.int)
                (Json.field "button" buttonDecoder)

        responseDecoder =
            Json.map2
                (\tag msg -> ( tag, msg ))
                (Json.field "tag" Json.string)
                (Json.at [ "args", "message" ] Json.string)
                |> Json.andThen
                    (\( tag, msg ) ->
                        if tag == "messageReceived" then
                            Json.succeed msg
                        else
                            Json.fail tag
                    )
    in
    Json.decodeValue responseDecoder value
        |> Result.andThen (Json.decodeString controlDecoder)



-- UPDATE


type Msg
    = Receive Value
    | Process Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Process value ->
            model
                |> Debug.log ("Process: " ++ encode 0 value)
                |> withCmd (cmdPort value)

        Receive value ->
            case control value of
                Err err ->
                    model
                        |> Debug.log (Json.errorToString err)
                        |> withNoCmd

                Ok ctrl ->
                    { model
                        | x =
                            case ctrl.button of
                                Left ->
                                    model.x - 5

                                Right ->
                                    model.x + 5

                                Shoot ->
                                    model.x
                    }
                        |> withNoCmd



-- VIEW


monsterView : Int -> Int -> ( Int, Int ) -> Svg msg
monsterView n width ( x, y ) =
    Svg.image
        [ Svg.xlinkHref ("/res/monster/" ++ String.fromInt n ++ ".png")
        , Svg.x (String.fromInt x)
        , Svg.y (String.fromInt y)
        , Svg.width (String.fromInt width)
        ]
        []


view : Model -> Html Msg
view model =
    Svg.svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 100" ]
        [ Svg.rect [ Svg.fill "gray", Svg.width "100", Svg.height "100" ] []
        , monsterView 0 20 ( model.x, 40 )
        , monsterView 1 20 ( 20, 5 )
        , monsterView 2 20 ( 40, 5 )
        , monsterView 3 20 ( 60, 5 )
        ]
