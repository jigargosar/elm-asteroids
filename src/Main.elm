module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (style)
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
                [ Svg.circle [ S.r "100" ] []
                , viewShip 10 -50 (turns -0.1)
                ]
            ]
        ]


viewShip x y a =
    Svg.g [ transform [ translate x y, rotate a ] ] [ ship ]


translate x y =
    "translate(" ++ String.fromFloat x ++ "px," ++ String.fromFloat y ++ "px)"


rotate a =
    "rotate(" ++ String.fromFloat a ++ "rad)"


transform =
    String.join " " >> style "transform"


shipR =
    20


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
            [ S.points (points [ tail1, head, tail2, innerTail ]) ]
            []
        ]


points =
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
