module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Generator)
import Svg exposing (Svg, svg)
import Svg.Attributes as S exposing (fill, stroke, viewBox)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    , left : Bool
    , right : Bool
    , forward : Bool
    }


type Msg
    = GotDelta Float
    | GotKeyDown String
    | GotKeyUp String


init : () -> ( Model, Cmd Msg )
init () =
    ( { p = ( 10, -50 )
      , a = turns 0.5
      , v = fromPolar ( 50, turns 0.5 )
      , left = False
      , right = False
      , forward = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta GotDelta
    , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map GotKeyDown)
    , Browser.Events.onKeyUp (JD.field "key" JD.string |> JD.map GotKeyUp)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        GotKeyDown "ArrowLeft" ->
            ( { m | left = True }, Cmd.none )

        GotKeyDown "ArrowRight" ->
            ( { m | right = True }, Cmd.none )

        GotKeyDown "ArrowUp" ->
            ( { m | forward = True }, Cmd.none )

        GotKeyDown _ ->
            ( m, Cmd.none )

        GotKeyUp "ArrowLeft" ->
            ( { m | left = False }, Cmd.none )

        GotKeyUp "ArrowRight" ->
            ( { m | right = False }, Cmd.none )

        GotKeyUp "ArrowUp" ->
            ( { m | forward = False }, Cmd.none )

        GotKeyUp _ ->
            ( m, Cmd.none )

        GotDelta dm ->
            let
                d =
                    dm / 1000
            in
            ( step d m, Cmd.none )


step : Float -> Model -> Model
step d m =
    { m
        | p =
            m.p
                |> vAdd (m.v |> vScale d)
                |> warpIn ( 500, 500 )
        , v =
            (if m.forward then
                vAdd m.v (fromPolar ( d * 50, m.a ))

             else
                m.v |> vMapMag (mul 0.9999)
            )
                |> vMapMag (atMost 100)
        , a =
            let
                angularDirection =
                    if m.left && not m.right then
                        -1

                    else if not m.left && m.right then
                        1

                    else
                        0
            in
            m.a + d * angularDirection * turns 0.5
    }


warpIn ( w, h ) ( x, y ) =
    ( if x < -w / 2 then
        w / 2

      else if x > w / 2 then
        -w / 2

      else
        x
    , if y < -h / 2 then
        h / 2

      else if y > h / 2 then
        -h / 2

      else
        y
    )


atMost =
    min


vMapMag f =
    toPolar >> Tuple.mapFirst f >> fromPolar


vAdd v p =
    map2 add p v


vScale n =
    map (mul n)


add =
    (+)


mul =
    (*)


map f ( a, b ) =
    ( f a, f b )


map2 f ( a, b ) ( c, d ) =
    ( f a c, f b d )


view : Model -> Html.Html msg
view m =
    div
        [ style "display" "grid"
        , style "height" "100%"
        , style "padding" "2rem"
        , style "box-sizing" "border-box"
        ]
        [ globalStyles
        , svg
            [ viewBox "0 0 500 500"
            , style "display" "block"
            , style "background-color" "black"
            , style "max-width" "100%"
            , style "max-height" "100%"
            , style "place-self" "center"
            , stroke "white"
            , S.strokeLinecap "round"
            , S.strokeDasharray "40 1"
            , fill "transparent"
            ]
            [ Svg.g
                [ style "transform" "translate(50%, 50%)"
                ]
                [ viewPA ship m.p m.a
                , viewXYA asteroidLarge -170 -70 (turns -0.1)
                , viewXYA asteroidSmall -100 70 (turns 0.1)
                ]
            ]
        ]


shipR =
    15


asteroidLargeR =
    60


asteroidSmallR =
    30


viewPA el ( x, y ) a =
    viewXYA el x y a


viewXYA imageEl x y a =
    Svg.g [ transformXYA x y a ] [ imageEl ]


asteroidLarge =
    let
        r =
            asteroidLargeR

        pts =
            Random.step (randomNgonPoints 0.1 25 r)
                (Random.initialSeed 0)
                |> Tuple.first
    in
    Svg.polygon
        [ attrPoints pts ]
        []


asteroidSmall =
    let
        r =
            asteroidSmallR

        pts =
            Random.step (randomNgonPoints 0.1 25 r)
                (Random.initialSeed 0)
                |> Tuple.first
    in
    Svg.polygon
        [ attrPoints pts ]
        []


randomNgonPoints deviation sides radius =
    let
        angles =
            splitTurn sides

        randomRadii =
            deviateBy deviation radius
    in
    Random.list sides randomRadii
        |> Random.map (List.map2 fromAngleRadius angles)


deviateBy : Float -> Float -> Generator Float
deviateBy d n =
    Random.float -d d
        |> Random.map (\rd -> rd * n + n)


splitTurn count =
    let
        toAngle i =
            turns (toFloat i / toFloat count)
    in
    List.map toAngle (List.range 1 count)


fromAngleRadius a r =
    fromPolar ( r, a )


ship : Svg msg
ship =
    let
        r =
            shipR

        head =
            ( r * 1.5, 0 )

        tailTop =
            ( r * -1.2, -r * 1.2 )

        tailBottom =
            tailTop |> Tuple.mapSecond negate

        tailMiddle =
            ( r * -0.5, 0 )

        pts =
            [ tailTop, head, tailBottom, tailMiddle ]
    in
    Svg.g []
        [ Svg.circle [ S.r (String.fromFloat r), stroke "green" ] []
            |> hide
        , Svg.polygon [ attrPoints pts ] []
        ]


hide =
    always (text "")


attrPoints : List ( Float, Float ) -> Svg.Attribute msg
attrPoints =
    List.map (\( a, b ) -> String.fromFloat a ++ "," ++ String.fromFloat b)
        >> String.join " "
        >> S.points


transformXYA x y a =
    transform [ translate x y, rotate a ]


transform =
    String.join " " >> style "transform"


translate x y =
    "translate(" ++ String.fromFloat x ++ "px," ++ String.fromFloat y ++ "px)"


rotate a =
    "rotate(" ++ String.fromFloat a ++ "rad)"


globalStyles =
    Html.node "style"
        []
        [ text """
            :root{height:100%;}
            body{height:100%;}
            polygon, circle{
                stroke-width:1.5;
                stroke-dasharray:1 2;
                stroke-linecap:round;
            }
            """
        ]
