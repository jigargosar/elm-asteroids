module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import List.Extra
import Random exposing (Generator, Seed)
import Random.Extra
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
    , rocks : List Rock
    , bullets : ( Float, List Bullet )
    , explosions : List Explosion
    , left : Bool
    , right : Bool
    , forward : Bool
    , trigger : Bool
    , seed : Seed
    }


type Explosion
    = Explosion


type alias Rock =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    , t : RockType
    }


isRockSmall : Rock -> Bool
isRockSmall rock =
    rock.t == RockSmall


rockSpeed =
    30


placeRockOutSideRoom : Rock -> Rock
placeRockOutSideRoom rock =
    let
        ( _, y ) =
            rock.p

        nx =
            roomInset.left - rockWarpMargin rock
    in
    { rock | p = ( nx, y ) }


type RockType
    = RockSmall
    | RockLarge


type alias Bullet =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    }


type Msg
    = GotDelta Float
    | GotKey Bool String


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 2

        ( rocks, seed ) =
            Random.step randomInitialRocks initialSeed
    in
    ( { p = ( 10, -50 )
      , a = turns 0.5
      , v = fromPolar ( 50, turns 0.5 )
      , rocks = rocks
      , bullets = ( 0, [] )
      , explosions = []
      , left = False
      , right = False
      , forward = False
      , trigger = False
      , seed = seed
      }
    , Cmd.none
    )


randomInitialRocks : Generator (List Rock)
randomInitialRocks =
    Random.list 4 randomInitialLargeRock


randomInitialLargeRock : Generator Rock
randomInitialLargeRock =
    Random.map4 Rock
        randomPointInRoom
        randomAngle
        randomRockVelocity
        (Random.constant RockLarge)


randomNewLargeRock =
    Random.map4 Rock
        randomPointInRoom
        randomAngle
        randomRockVelocity
        (Random.constant RockLarge)
        |> Random.map placeRockOutSideRoom


velocityFromSpeedAndAngle r a =
    fromPolar ( r, a )


randomRockVelocity =
    randomAngle
        |> Random.map (velocityFromSpeedAndAngle rockSpeed)


randomAngle =
    Random.float 0 (turns 1)


randomPointInRoom =
    let
        ( w, h ) =
            roomSize
    in
    Random.pair
        (Random.float (-w / 2) (w / 2))
        (Random.float (-h / 2) (h / 2))


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta GotDelta
    , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map (GotKey True))
    , Browser.Events.onKeyUp (JD.field "key" JD.string |> JD.map (GotKey False))
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        GotKey isDown "ArrowLeft" ->
            ( { m | left = isDown }, Cmd.none )

        GotKey isDown "ArrowRight" ->
            ( { m | right = isDown }, Cmd.none )

        GotKey isDown "ArrowUp" ->
            ( { m | forward = isDown }, Cmd.none )

        GotKey isDown " " ->
            ( { m | trigger = isDown }, Cmd.none )

        GotKey _ _ ->
            ( m, Cmd.none )

        GotDelta dm ->
            let
                d =
                    dm / 1000
            in
            ( step d m, Cmd.none )


exp n =
    e ^ n


step : Float -> Model -> Model
step d m =
    { m
        | p =
            m.p
                |> vAdd (m.v |> vScale d)
                |> warp ( 500, 500 )
        , v =
            m.v
                |> friction d 0.05
                |> (if m.forward then
                        vAdd (fromPolar ( d * 100, m.a ))

                    else
                        identity
                   )
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
        , rocks = List.map (stepRock d) m.rocks
        , bullets =
            let
                ( elapsed, bullets ) =
                    m.bullets

                updatedBullets =
                    List.filterMap (stepBullet d) bullets
            in
            if m.trigger && elapsed > 0.5 then
                ( 0
                , { p = m.p, a = m.a, v = fromPolar ( 300, m.a ) } :: updatedBullets
                )

            else
                ( elapsed + d
                , updatedBullets
                )
    }
        |> collision


roomWidth =
    500


roomHeight =
    500


roomSize =
    ( roomWidth, roomHeight )


roomHalfSize =
    map (mul 0.5) roomSize


roomInset =
    let
        ( hw, hh ) =
            roomHalfSize
    in
    { top = -hh
    , right = hw
    , bottom = hh
    , left = -hw
    }


collision : Model -> Model
collision m =
    let
        ( safeBullets, ( rocksHit, rocksSafe ) ) =
            bulletsRocksCollision (Tuple.second m.bullets) m.rocks

        ( shatteredRocks, seed ) =
            randomBreakLargeRocks rocksHit
                |> randomStepWithSeed m.seed

        smallRocksHitCount =
            List.Extra.count isRockSmall rocksHit
    in
    { m
        | rocks = rocksSafe ++ shatteredRocks
        , explosions = [ Explosion ]
        , bullets = Tuple.mapSecond (always safeBullets) m.bullets
        , seed = seed
    }
        |> addNewRocks smallRocksHitCount


addNewRocks : Int -> Model -> Model
addNewRocks amount m =
    let
        capacity =
            (maxRocksCount - List.length m.rocks)
                |> atLeast 0

        finalAmount =
            min amount capacity

        ( newRocks, seed ) =
            Random.step (Random.list finalAmount randomNewLargeRock)
                m.seed
    in
    { m
        | rocks = m.rocks ++ newRocks
        , seed = seed
    }


atLeast =
    max


maxRocksCount =
    16


listCount pred =
    List.filter pred >> List.length


randomBreakLargeRocks rocks =
    List.concatMap
        (\rock ->
            case rock.t of
                RockSmall ->
                    []

                RockLarge ->
                    [ { rock | t = RockSmall }, { rock | t = RockSmall } ]
        )
        rocks
        |> List.map randomizeRockVelocity
        |> Random.Extra.combine


randomStepWithSeed seed gen =
    Random.step gen seed


randomizeRockVelocity : Rock -> Generator Rock
randomizeRockVelocity rock =
    randomAngle
        |> Random.map (\a -> { rock | v = vMapAngle (always a) rock.v })


vMapAngle f =
    toPolar >> Tuple.mapSecond f >> fromPolar


bulletsRocksCollision : List Bullet -> List Rock -> ( List Bullet, ( List Rock, List Rock ) )
bulletsRocksCollision initialBullets initialRocks =
    List.foldl
        (\rock ( bullets, ( dead, safe ) ) ->
            case removeFirst (rockBulletCollision rock) bullets of
                Just ( _, safeBullets ) ->
                    ( safeBullets, ( enq rock dead, safe ) )

                Nothing ->
                    ( bullets, ( dead, enq rock safe ) )
        )
        ( initialBullets, ( emptyQ, emptyQ ) )
        initialRocks
        |> Tuple.mapSecond (map qToList)


removeFirst : (a -> Bool) -> List a -> Maybe ( a, List a )
removeFirst pred list =
    removeFirstHelp pred list emptyQ


removeFirstHelp : (a -> Bool) -> List a -> Q a -> Maybe ( a, List a )
removeFirstHelp pred pending done =
    case pending of
        [] ->
            Nothing

        h :: t ->
            if pred h then
                Just ( h, qToList done ++ t )

            else
                removeFirstHelp pred t (enq h done)


type Q a
    = Q (List a)


emptyQ =
    Q []


enq : a -> Q a -> Q a
enq a (Q list) =
    Q (a :: list)


qToList : Q a -> List a
qToList (Q list) =
    List.reverse list


rockBulletCollision : Rock -> Bullet -> Bool
rockBulletCollision rock bullet =
    distanceSquared rock.p bullet.p < (rockCollisionRadius rock ^ 2)


rockCollisionRadius : Rock -> Float
rockCollisionRadius rock =
    rockRadius rock


rockRadius : Rock -> Float
rockRadius rock =
    case rock.t of
        RockSmall ->
            rockSmallRadius

        RockLarge ->
            rockLargeRadius


distanceSquared ( a, b ) ( c, d ) =
    ((a - c) ^ 2) + ((b - d) ^ 2)


friction d coefficient =
    --https://gamedev.net/forums/topic/382585-friction-and-frame-independant-motion/382585
    vScale (exp (-d * coefficient))


stepBullet : Float -> Bullet -> Maybe Bullet
stepBullet d bullet =
    if withinBounds ( 500, 500 ) bullet.p then
        Just { bullet | p = bullet.p |> vAdd (bullet.v |> vScale d) }

    else
        Nothing


withinBounds ( w, h ) ( x, y ) =
    x > -w / 2 && x < w / 2 && y > -h / 2 && y < h / 2


stepRock : Float -> Rock -> Rock
stepRock d rock =
    { rock
        | p =
            rock.p
                |> vAdd (rock.v |> vScale d)
                |> warpWithMargin (rockWarpMargin rock) roomSize
        , a = rock.a + d * turns 0.1
    }


rockWarpMargin rock =
    rockRadius rock * 1.1


warpWithMargin margin size =
    let
        sizeWithMargin =
            map (add (margin * 2)) size
    in
    warp sizeWithMargin


warp ( w, h ) ( x, y ) =
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
                ([ viewPA ship m.p m.a ]
                    ++ List.map viewRock m.rocks
                    ++ List.map
                        (\bullet -> viewPA bulletShape bullet.p bullet.a)
                        (Tuple.second m.bullets)
                )
            ]
        ]


viewRock : Rock -> Svg msg
viewRock rock =
    viewPA
        (case rock.t of
            RockSmall ->
                rockSmallSvg

            RockLarge ->
                rockLargeSvg
        )
        rock.p
        rock.a


bulletShape =
    Svg.circle [ S.r "3", fill "white", stroke "none" ] []


shipR =
    15


rockLargeRadius =
    60


rockSmallRadius =
    30


viewPA el ( x, y ) a =
    viewXYA el x y a


viewXYA imageEl x y a =
    Svg.g [ transformXYA x y a ] [ imageEl ]


rockLargeSvg =
    let
        r =
            rockLargeRadius

        pts =
            Random.step (randomNgonPoints 0.05 30 r)
                (Random.initialSeed 0)
                |> Tuple.first
    in
    Svg.polygon
        [ attrPoints pts ]
        []


rockSmallSvg =
    let
        r =
            rockSmallRadius

        pts =
            Random.step (randomNgonPoints 0.07 30 r)
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
