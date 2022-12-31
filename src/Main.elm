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
        [ Html.node "style"
            []
            [ text """
                :root{height:100%;}
                body{height:100%;}
                """
            ]
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
                ]
            ]
        ]
