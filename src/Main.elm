module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg exposing (svg)
import Svg.Attributes as S exposing (fill, stroke, viewBox)


main =
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
            , fill "transparent"
            ]
            [ Svg.g
                [ style "transform" "translate(50%, 50%)"
                ]
                [ viewXYA ship 10 -50 (turns -0.1)
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


viewXYA imageEl x y a =
    Svg.g [ transform [ translate x y, rotate a ] ] [ imageEl ]


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
        [ S.points (pointsAsString pts) ]
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
        [ S.points (pointsAsString pts) ]
        []


randomNgonPoints deviation sides radius =
    let
        angles =
            splitTurn sides

        randomRadii =
            deviateFloatBy deviation radius
    in
    Random.list sides randomRadii
        |> Random.map (List.map2 fromAngleRadius angles)


deviateFloatBy : Float -> Float -> Generator Float
deviateFloatBy d f =
    Random.float -d d
        |> Random.map (\rd -> rd * f + f)


splitTurn count =
    let
        toAngle i =
            turns (toFloat i / toFloat count)
    in
    List.map toAngle (List.range 1 count)


fromAngleRadius a r =
    fromPolar ( r, a )


translate x y =
    "translate(" ++ String.fromFloat x ++ "px," ++ String.fromFloat y ++ "px)"


rotate a =
    "rotate(" ++ String.fromFloat a ++ "rad)"


transform =
    String.join " " >> style "transform"


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
        , Svg.polygon
            [ S.points (pointsAsString pts) ]
            []
        ]


pointsAsString =
    List.map (\( a, b ) -> String.fromFloat a ++ "," ++ String.fromFloat b)
        >> String.join " "


globalStyles =
    Html.node "style"
        []
        [ text """
                              :root{height:100%;}
                              body{height:100%;}
                              """
        ]
