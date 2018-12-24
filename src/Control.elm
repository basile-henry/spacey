module Control exposing (..)

import Json.Decode as Json
import Json.Encode exposing (Value)


type Button
    = Left
    | Shoot
    | Right


type State
    = Up
    | Down


type alias Control =
    { player : Int
    , button : Button
    , state : State
    }


control : Value -> Result Json.Error Control
control value =
    let
        buttonDecoder =
            Json.andThen
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
                Json.string

        stateDecoder =
            Json.andThen
                (\state ->
                    case state of
                        "Up" ->
                            Json.succeed Up

                        "Down" ->
                            Json.succeed Down

                        _ ->
                            Json.fail state
                )
                Json.string

        controlDecoder =
            Json.map3
                Control
                (Json.field "index" Json.int)
                (Json.field "button" buttonDecoder)
                (Json.field "state" stateDecoder)

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
