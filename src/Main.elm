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
    { ship : Ship
    , rocks : List Rock
    , bullets : ( Float, List Bullet )
    , explosions : List Explosion
    , input : Input
    , seed : Seed
    }


type alias Input =
    { left : Bool
    , right : Bool
    , forward : Bool
    , trigger : Bool
    }


inputUpdate : Bool -> String -> Input -> Input
inputUpdate isDown key input =
    case key of
        "ArrowLeft" ->
            { input | left = isDown }

        "ArrowRight" ->
            { input | right = isDown }

        "ArrowUp" ->
            { input | forward = isDown }

        " " ->
            { input | trigger = isDown }

        _ ->
            input


type alias Ship =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    }


shipInitial : Ship
shipInitial =
    { p = ( 10, -50 )
    , a = turns 0.5
    , v = fromPolar ( 50, turns 0.5 )
    }


shipStep : Float -> Input -> Ship -> Ship
shipStep d input m =
    { p =
        m.p
            |> vAdd (m.v |> vScale d)
            |> warpInDimension roomSize
    , v =
        m.v
            |> friction d 0.05
            |> (if input.forward then
                    vAdd (fromPolar ( d * 100, m.a ))

                else
                    identity
               )
    , a =
        let
            angularDirection =
                if input.left && not input.right then
                    -1

                else if not input.left && input.right then
                    1

                else
                    0
        in
        m.a + d * angularDirection * turns 0.5
    }


type alias Explosion =
    { elapsed : Float
    , duration : Float
    , rock : Rock
    }


type alias Rock =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    , t : RockType
    }


type RockType
    = RockSmall
    | RockLarge


rockIsSmall : Rock -> Bool
rockIsSmall rock =
    rock.t == RockSmall


rockSpeed =
    30


rockPlaceOutSideRoom : Rock -> Rock
rockPlaceOutSideRoom rock =
    let
        ( _, y ) =
            rock.p

        ( rockWidth, _ ) =
            rockSize rock

        nx =
            roomInset.left - (rockWidth * 1.1)
    in
    { rock | p = ( nx, y ) }


rockSize rock =
    let
        diameter =
            rockRadius rock * 2
    in
    ( diameter, diameter )


type alias Bullet =
    { p : ( Float, Float )
    , a : Float
    , v : ( Float, Float )
    }


type Msg
    = GotAnimationFrame Float
    | GotKey Bool String


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 2

        ( rocks, seed ) =
            Random.step randomInitialRocks initialSeed
    in
    ( { ship = shipInitial
      , rocks = rocks
      , bullets = ( 0, [] )
      , explosions = []
      , input =
            { left = False
            , right = False
            , forward = False
            , trigger = False
            }
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
        |> Random.map rockPlaceOutSideRoom


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
    [ Browser.Events.onAnimationFrameDelta GotAnimationFrame
    , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map (GotKey True))
    , Browser.Events.onKeyUp (JD.field "key" JD.string |> JD.map (GotKey False))
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        GotKey isDown key ->
            ( { m | input = inputUpdate isDown key m.input }, Cmd.none )

        GotAnimationFrame deltaMilli ->
            let
                deltaSeconds =
                    atMost 100 deltaMilli / 1000
            in
            ( step deltaSeconds m, Cmd.none )


step : Float -> Model -> Model
step d m =
    { m
        | ship = shipStep d m.input m.ship
        , rocks = List.map (stepRock d) m.rocks
        , explosions = List.filterMap (stepExplosion d) m.explosions
        , bullets = stepBullets d m.input m.ship m.bullets
    }
        |> collision


stepBullets d input ship ( elapsed, bullets ) =
    let
        updatedBullets =
            List.filterMap (stepBullet d) bullets
    in
    if input.trigger && elapsed > 0.5 then
        ( 0
        , { p = ship.p, a = ship.a, v = fromPolar ( 300, ship.a ) } :: updatedBullets
        )

    else
        ( elapsed + d
        , updatedBullets
        )


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
            List.Extra.count rockIsSmall rocksHit
    in
    { m
        | rocks = rocksSafe ++ shatteredRocks
        , explosions = m.explosions ++ explodeRocks rocksHit
        , bullets = Tuple.mapSecond (always safeBullets) m.bullets
        , seed = seed
    }
        |> addNewRocks smallRocksHitCount


explodeRocks : List Rock -> List Explosion
explodeRocks =
    List.map (\rock -> { elapsed = 0, duration = 0.25, rock = rock })


stepExplosion : Float -> Explosion -> Maybe Explosion
stepExplosion d e =
    let
        elapsed =
            e.elapsed + d
    in
    if elapsed > e.duration then
        Nothing

    else
        Just { e | elapsed = elapsed }


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


atMost =
    min


maxRocksCount =
    16


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
    vScale (e ^ (-d * coefficient))


stepBullet : Float -> Bullet -> Maybe Bullet
stepBullet d bullet =
    if withinBounds roomSize bullet.p then
        Just { bullet | p = bullet.p |> vAdd (bullet.v |> vScale d) }

    else
        Nothing


withinBounds ( w, h ) ( x, y ) =
    x > -w / 2 && x < w / 2 && y > -h / 2 && y < h / 2


stepRock : Float -> Rock -> Rock
stepRock d rock =
    let
        grownRoomSize =
            vAdd roomSize (rockSize rock |> vScale 1.1)
    in
    { rock
        | p =
            rock.p
                |> vAdd (rock.v |> vScale d)
                |> warpInDimension grownRoomSize
        , a = rock.a + d * turns 0.1
    }


warpInDimension ( w, h ) ( x, y ) =
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
            [ attrViewBox (roomSize |> vScale 1)
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
            ([ rect roomSize []
             , viewShip m.ship
             ]
                ++ List.map viewRock m.rocks
                ++ List.map viewExplosion m.explosions
                ++ List.map
                    (\bullet -> viewPA bulletShape bullet.p bullet.a)
                    (Tuple.second m.bullets)
            )
        ]


viewShip : Ship -> Svg msg
viewShip ship =
    viewPA shipSvg ship.p ship.a


viewExplosion e =
    let
        lifetime =
            e.elapsed / e.duration |> clamp 0 1

        fading =
            0.7 - (lifetime * 0.7)

        scaling =
            0.9 + (lifetime * 0.4)
    in
    Svg.g
        [ S.opacity (String.fromFloat fading)
        , transform [ scale scaling ]
        , style "transform-origin" "center"
        , style "transform-box" "fill-box"
        ]
        [ viewRock e.rock ]


rect size attrs =
    let
        ( x, y ) =
            size |> map (mul -0.5 >> String.fromFloat)

        ( w, h ) =
            size |> map String.fromFloat
    in
    Svg.rect (S.x x :: S.y y :: S.width w :: S.height h :: attrs) []


attrViewBox size =
    let
        ( w, h ) =
            size
    in
    viewBox (String.join " " (List.map String.fromFloat [ -w / 2, -h / 2, w, h ]))


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
    10


rockLargeRadius =
    40


rockSmallRadius =
    20


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


shipSvg : Svg msg
shipSvg =
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


scale s =
    [ "scale(", String.fromFloat s, ")" ]
        |> String.join ""


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
