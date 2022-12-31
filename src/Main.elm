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
    20


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


randomFloatWithUniformDeviation : Float -> Float -> Generator Float
randomFloatWithUniformDeviation d f =
    Random.float -d d
        |> Random.map (\rd -> rd * f + f)


randomNgonPoints d s r =
    let
        angles =
            splitTurn s
    in
    Random.list s (randomFloatWithUniformDeviation d r)
        |> Random.map (\radii -> List.map2 fromRadiusAngle radii angles)


splitTurn s =
    List.range 1 s
        |> List.map
            (toFloat
                >> (\n ->
                        turns (n / toFloat s)
                   )
            )


fromRadiusAngle r a =
    fromPolar ( r, a )



--ngonPoints s r =
--    let
--        fromAngle a =
--            fromPolar ( r, a )
--    in
--    List.map fromAngle (splitTurn s)


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

        tail1 =
            ( r * -1.2, -r )

        tail2 =
            tail1 |> Tuple.mapSecond negate

        innerTail =
            ( r * -0.5, 0 )
    in
    Svg.g []
        [ Svg.circle [ S.r (String.fromFloat r), stroke "green" ] []
        , Svg.polygon
            [ S.points (pointsAsString [ tail1, head, tail2, innerTail ]) ]
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
