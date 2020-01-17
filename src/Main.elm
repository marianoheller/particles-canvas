module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced as Advanced
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events.Extra.Mouse as Mouse
import Math.Vector2 exposing (..)
import Random as Random
import Task as Task



-- CONFIG


qtyParticlesMin : Int
qtyParticlesMin =
    40


logoUrl : String
logoUrl =
    "assets/img/logo.png"


mapUrl : String
mapUrl =
    "assets/img/map.png"



-- TYPES


type alias Window =
    { width : Float
    , height : Float
    }


type alias Position =
    Vec2


type alias Direction =
    Vec2


type alias Radius =
    Float


type Particle
    = Particle Position Direction Radius


type alias Model =
    { particles : List Particle
    , window : Window
    , logo : Maybe Texture
    , map : Maybe Texture
    }


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        width =
            flags.windowWidth

        height =
            flags.windowHeight
    in
    ( { particles = []
      , window =
            { width = flags.windowWidth
            , height = flags.windowHeight
            }
      , logo = Nothing
      , map = Nothing
      }
    , Cmd.batch
        [ Task.perform GotViewport getViewport
        , Random.generate Populate <|
            Random.list qtyParticlesMin (tupleInitGenerator width height)
        ]
    )



-- UPDATE


type Msg
    = Frame Float
    | GotViewport Viewport
    | Populate (List ( Position, Direction, Radius ))
    | CanvasClick ( Float, Float )
    | LogoLoaded (Maybe Texture)
    | MapLoaded (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame delta ->
            let
                newParticles =
                    processParticles model.window delta model.particles

                qty =
                    List.length newParticles

                cmdGenerateParticle =
                    if qty >= qtyParticlesMin then
                        Cmd.none

                    else
                        Random.generate Populate <|
                            Random.list (qtyParticlesMin - qty)
                                (tupleGenerator
                                    model.window.width
                                    model.window.height
                                )
            in
            ( { model | particles = newParticles }
            , cmdGenerateParticle
            )

        GotViewport viewport ->
            ( { model
                | window =
                    { width = viewport.viewport.width
                    , height = viewport.viewport.height
                    }
              }
            , Cmd.none
            )

        Populate listTuples ->
            ( { model
                | particles =
                    List.append model.particles <|
                        List.map createFromTuple listTuples
              }
            , Cmd.none
            )

        CanvasClick ( x, y ) ->
            ( model, Random.generate Populate <| Random.list 3 <| tupleGeneratorAt x y )

        LogoLoaded maybeLogo ->
            ( { model | logo = maybeLogo }, Cmd.none )

        MapLoaded maybeMap ->
            ( { model | map = maybeMap }, Cmd.none )


processParticles : Window -> Float -> List Particle -> List Particle
processParticles window delta =
    List.map (updatePosition delta) << filterOutOffscreenParticles window


filterOutOffscreenParticles : Window -> List Particle -> List Particle
filterOutOffscreenParticles { height, width } =
    let
        filterOut (Particle pos _ radius) =
            not <|
                (getX pos - radius)
                    > width
                    || (getX pos + radius)
                    < 0
                    || (getY pos - radius)
                    > height
                    || (getY pos + radius)
                    < 0
    in
    List.filter filterOut


updatePosition : Float -> Particle -> Particle
updatePosition delta (Particle pos dir radius) =
    let
        -- Pixels per sec
        velPerSec =
            100

        deltaPos =
            normalize dir |> scale ((velPerSec / 1000) * delta)
    in
    Particle (add pos deltaPos) dir radius


createFromTuple : ( Position, Direction, Radius ) -> Particle
createFromTuple ( position, direction, radius ) =
    Particle position direction radius



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            model.window.width

        height =
            model.window.height
    in
    Canvas.toHtmlWith
        { width = round width
        , height = round height
        , textures =
            [ loadFromImageUrl logoUrl LogoLoaded
            , loadFromImageUrl mapUrl MapLoaded
            ]
        }
        [ Mouse.onDown (\event -> CanvasClick event.offsetPos) ]
    <|
        [ shapes
            [ fill <| Color.rgb 0.4 0.4 0.4 ]
            [ rect ( 0, 0 ) width height ]
        ]
            ++ ([ drawConnections model.particles ]
                    ++ drawMap model.map
                    ++ List.map drawParticle model.particles
                    ++ drawLogo width height model.logo
               )


drawMap : Maybe Texture -> List Renderable
drawMap maybeMap =
    case maybeMap of
        Nothing ->
            []

        Just map ->
            [ texture
                []
                -- Advanced.transform [Advanced.scale 0.9 0.9]
                ( 0, 0 )
                map
            ]


drawLogo : Float -> Float -> Maybe Texture -> List Renderable
drawLogo canvasWidth canvasHeight maybeLogo =
    case maybeLogo of
        Nothing ->
            []

        Just logo ->
            let
                { width, height } =
                    dimensions logo
            in
            [ texture
                []
                ( (canvasWidth / 2) - (width / 2), (canvasHeight / 2) - (height / 2) )
                logo
            ]


colorParticle : Color.Color
colorParticle =
    Color.rgb 0.6 0.6 0.6


colorConnection : Color.Color
colorConnection =
    Color.rgba 0.6 0.6 0.6 0.2


drawParticle : Particle -> Renderable
drawParticle (Particle pos _ radius) =
    shapes [ fill colorParticle ]
        [ circle ( getX pos, getY pos ) radius
        ]


drawConnections : List Particle -> Renderable
drawConnections particles =
    let
        minDistance =
            150

        drawConnection from to =
            path ( getX from, getY from )
                [ lineTo ( getX to, getY to )
                ]

        folder (Particle pos _ _) acc =
            acc
                ++ (List.filter (\(Particle pos2 _ _) -> distance pos pos2 < minDistance)
                        particles
                        |> List.map (\(Particle pos2 _ _) -> drawConnection pos pos2)
                   )
    in
    shapes
        [ stroke colorConnection
        , lineWidth 1
        ]
    <|
        List.foldr folder [] particles



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame



-- MISC


vec2FromTuple : ( Float, Float ) -> Vec2
vec2FromTuple ( x, y ) =
    vec2 x y


genRadius : Random.Generator Radius
genRadius =
    Random.float 1 3


tupleInitGenerator : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleInitGenerator width height =
    let
        genX =
            Random.float 0 width

        genY =
            Random.float 0 height

        genPos =
            Random.map vec2FromTuple <| Random.pair genX genY

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    randomPair3 genPos genDir genRadius


tupleGenerator : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleGenerator width height =
    let
        borderPosGen =
            Random.uniform
                (Random.pair
                    (Random.uniform 0 [ width ])
                    (Random.float 0 height)
                )
                [ Random.pair (Random.float 0 width) (Random.uniform 0 [ height ]) ]

        genGenPos =
            (Random.map << Random.map) vec2FromTuple <| borderPosGen

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    genGenPos
        |> Random.andThen (\genPos -> randomPair3 genPos genDir genRadius)


tupleGeneratorAt : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleGeneratorAt x y =
    let
        genX =
            Random.constant x

        genY =
            Random.constant y

        genPos =
            Random.map vec2FromTuple <| Random.pair genX genY

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    randomPair3 genPos genDir genRadius


randomPair3 :
    Random.Generator a
    -> Random.Generator b
    -> Random.Generator c
    -> Random.Generator ( a, b, c )
randomPair3 genA genB genC =
    Random.map3 (\a b c -> ( a, b, c )) genA genB genC



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
