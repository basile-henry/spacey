port module Main exposing (main)

import Browser
import Browser.Events
import Cmd.Extra exposing (withCmd, withNoCmd)
import Control
import Html exposing (Html)
import Html.Attributes as Html
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
    , projectiles : List Projectile
    , explosions : List Explosion
    }


playerWidth : Float
playerWidth =
    8


projectileLength : Float
projectileLength =
    5


initPlayers : List Player
initPlayers =
    List.range 0 3
        |> List.map
            (\i ->
                { x = 20 + toFloat i * 20 - playerWidth / 2
                , angle = 0.0
                , left = False
                , right = False
                , shoot = False
                , score = 0
                , reload = 0.0
                }
            )


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
      , projectiles = []
      , explosions = []
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
    , score : Int
    , reload : Float
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


type alias Projectile =
    { x : Float
    , y : Float
    , player : Int
    }


type alias Explosion =
    { x : Float
    , y : Float
    , width : Float
    , frameTime : Float
    , frame : Int
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
            withCmd (cmdPort value) model

        Receive value ->
            case Control.control value of
                Err _ ->
                    withNoCmd model

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


tickPlayer : Float -> Int -> Player -> ( Player, Maybe Projectile )
tickPlayer dt n player =
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

        ( projectile, reload ) =
            if player.shoot && player.reload == 0 then
                ( Just
                    { x = player.x + playerWidth / 2
                    , y = 87 + playerWidth / 2
                    , player = n
                    }
                , 1000
                )
            else
                ( Nothing, max 0.0 (player.reload - dt) )
    in
    ( { player | x = x, angle = angle, reload = reload }
    , projectile
    )


tickAlien : Float -> Alien -> Alien
tickAlien dt alien =
    let
        dx =
            dt / 200

        dy =
            dt / 2000
    in
    if alien.x > alien.maxX then
        { alien | x = alien.x - dx, dir = -1, y = alien.y + dy }
    else if alien.x < alien.minX then
        { alien | x = alien.x + dx, dir = 1, y = alien.y + dy }
    else
        { alien | x = alien.x + alien.dir * dx, y = alien.y + dy }


tickProjectile : Float -> Projectile -> Projectile
tickProjectile dt projectile =
    let
        dy =
            dt / 30
    in
    { projectile | y = projectile.y - dy }


tickExplosion : Float -> Explosion -> Maybe Explosion
tickExplosion dt explosion =
    let
        frameTime_ =
            explosion.frameTime + dt / 1000

        ( frame, frameTime ) =
            if frameTime_ > 0.3 then
                ( explosion.frame + 1
                , 0.0
                )
            else
                ( explosion.frame
                , frameTime_
                )
    in
    if frame == 3 then
        Nothing
    else
        Just
            { explosion
                | frameTime = frameTime
                , frame = frame
            }


tick : Float -> Model -> ( Model, Cmd msg )
tick dt model =
    let
        ( players, newProjectiles ) =
            model.players
                |> List.indexedMap (tickPlayer dt)
                |> List.unzip

        aliens =
            List.map (tickAlien dt) model.aliens

        projectiles =
            List.map (tickProjectile dt) model.projectiles
                ++ List.filterMap identity newProjectiles

        explosions =
            List.filterMap (tickExplosion dt) model.explosions
    in
    handleCollisions
        { model
            | players = players
            , aliens = aliens
            , projectiles = projectiles
            , explosions = explosions
        }
        |> withNoCmd


handleCollisions : Model -> Model
handleCollisions model =
    List.foldr
        handleCollision
        { model | projectiles = [] }
        model.projectiles


type Hit
    = NoHit
    | Hit
    | FinalHit Int Alien
    | Out


handleCollision :
    Projectile
    -> Model
    -> Model
handleCollision projectile model =
    let
        bottom =
            projectile.y + projectileLength

        collision alien ( aliens, hit ) =
            case hit of
                NoHit ->
                    let
                        xCheck =
                            projectile.x >= alien.x && projectile.x <= alien.x + alien.width

                        -- Assume projectile is smaller then aliens
                        -- and aliens are square
                        projTop =
                            projectile.y >= alien.y && projectile.y <= alien.y + alien.width

                        projBottom =
                            bottom >= alien.y && bottom <= alien.y + alien.width

                        yCheck =
                            projTop || projBottom
                    in
                    if xCheck && yCheck then
                        if alien.life == 1 then
                            ( aliens
                            , FinalHit projectile.player alien
                            )
                        else
                            ( { alien | life = alien.life - 1 } :: aliens
                            , Hit
                            )
                    else
                        ( alien :: aliens, hit )

                _ ->
                    ( alien :: aliens, hit )

        ( aliens_, hit_ ) =
            List.foldr collision ( [], NoHit ) model.aliens

        players =
            case hit_ of
                FinalHit n alien ->
                    List.update
                        n
                        (\p -> { p | score = p.score + alien.points })
                        model.players

                _ ->
                    model.players

        projectiles =
            case hit_ of
                NoHit ->
                    projectile :: model.projectiles

                _ ->
                    model.projectiles

        explosions =
            case hit_ of
                FinalHit _ alien ->
                    { x = alien.x
                    , y = alien.y
                    , width = alien.width
                    , frameTime = 0.0
                    , frame = 0
                    }
                        :: model.explosions

                _ ->
                    model.explosions
    in
    if bottom < 0 then
        model
    else
        { model
            | players = players
            , aliens = aliens_
            , projectiles = projectiles
            , explosions = explosions
        }



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


explosionView : Explosion -> Svg msg
explosionView explosion =
    let
        width =
            case explosion.frame of
                0 ->
                    explosion.width / 2

                1 ->
                    explosion.width * 3 / 4

                _ ->
                    explosion.width

        x =
            explosion.x + explosion.width / 2 - width / 2

        y =
            explosion.y + explosion.width / 2 - width / 2
    in
    Svg.image
        [ Svg.xlinkHref ("/res/explosion/" ++ String.fromInt explosion.frame ++ ".png")
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


projectileView : Projectile -> Svg msg
projectileView projectile =
    let
        color =
            case projectile.player of
                0 ->
                    "#63cf76"

                1 ->
                    "#d76464"

                2 ->
                    "#59a3dc"

                _ ->
                    "#f4e047"
    in
    Svg.rect
        [ Svg.x (String.fromFloat (projectile.x - 0.5))
        , Svg.y (String.fromFloat projectile.y)
        , Svg.width "1"
        , Svg.height (String.fromFloat projectileLength)
        , Svg.rx "0.5"
        , Svg.ry "0.5"
        , Svg.fill color
        ]
        []


gameView : Model -> Html Msg
gameView model =
    let
        monsters =
            List.map alienView model.aliens

        players =
            List.indexedMap playerView model.players

        projectiles =
            List.map projectileView model.projectiles

        explosions =
            List.map explosionView model.explosions

        background =
            Svg.image
                [ Svg.xlinkHref "/res/background.jpg"
                , Svg.width "100"
                , Svg.height "100"
                ]
                []
    in
    Svg.svg
        [ Svg.width "100%", Svg.height "100%", Svg.viewBox "0 0 100 100" ]
        (background
            :: monsters
            ++ projectiles
            ++ explosions
            ++ players
        )


scoreView : Model -> Html msg
scoreView model =
    let
        viewHelper n score =
            Html.div
                [ Html.style "flex" "1"
                , Html.style "display" "flex"
                ]
                [ Html.div
                    [ Html.style "font-size" "2em"
                    , Html.style "text-align" "right"
                    , Html.style "flex" "1"
                    ]
                    [ Html.text <| String.fromInt score
                    ]
                , Html.div [ Html.style "flex" "0.1" ] []
                , Html.img
                    [ Html.style "height" "3em"
                    , Html.src ("/res/ship/" ++ String.fromInt n ++ ".png")
                    ]
                    []
                ]

        scores =
            List.indexedMap (\n p -> viewHelper n p.score) model.players
    in
    Html.div [ Html.style "display" "flex" ] scores


view : Model -> Html Msg
view model =
    let
        title =
            Html.div
                [ Html.style "height" "3.5em"
                , Html.style "position" "relative"
                ]
                [ Html.h1
                    []
                    [ Html.text "Spacey" ]
                , Html.div
                    [ Html.style "position" "absolute"
                    , Html.style "right" "0px"
                    , Html.style "bottom" "0px"
                    ]
                    [ Html.h3 [] [ Html.text "By Bénédicte & Basile Henry" ] ]
                ]
    in
    Html.div
        [ Html.style "font-family" "Open Sans" ]
        [ title
        , gameView model
        , scoreView model
        ]
