-- | módulo para dibujar
module CirclesDraw exposing (..)
-- Importaciones para interactuar con el navegador
import Browser
import Browser.Dom as Dom
import Browser.Events as E
-- Importaciones para el manejo de archivos dentro de Elm
import File exposing (File)
import File.Download as Download
import File.Select as Select
-- Importaciones para la gestión de eventos y generación de Html 
import Html as Ht
import Html.Attributes as H
import Html.Events as He
-- Importación para el uso de círculos y pasarlos a Svg

-- Ver: https://github.com/evancz/elm-playground/tree/master

-- Le hice algunas modificaciones al código original para poder
-- exportar a SVG
import Playground as P exposing ( renderShape
                                , circle
                                , lightPurple
                                , moveX
                                , moveY
                                , fade)
-- Importaciones para el uso de Svg
import Svg exposing (..)
import Svg.Attributes as A exposing (..)
-- Importación para decodificar eventos del navegador
import Json.Decode as D
-- Importación para ejecutar comandos
import Task

-- Función main que inicia el entorno de navegador con las funciones
-- que le pasamos.
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions}

-- Tipo de dato Msg para la gestión de acciones del programa
type Msg = NewViewport Dom.Viewport
         | Resize Int Int
         | MouseMove Float Float
         | MouseButton Bool
         | Save
         | Load
         | Received File
         | Decoded String

-- Tipo de dato Screen para la gestión del tamaño de la ventana
type alias Screen =
    { width : Float
    , height : Float
    , top : Float
    , bottom : Float
    , left : Float
    , right : Float}

-- Función para generar una pantalla dados los parametros de anchura y
-- altura
toScreen w h =
    { width = w
    , height = h
    , top = h / 2
    , bottom = -h / 2
    , left = -w / 2
    , right = w / 2}

-- Tipo de dato Mouse para guardar su posición y si ha sido pulsado
type alias Mouse =
    { x : Float
    , y : Float
    , down : Bool}

-- Estado inicial del mouse
initMouse =
    { x = 0
    , y = 0
    , down = False }

-- Tipo de dato círculo definido por un identificador, un centro y su
-- radio
type alias Circle =
    { did : Int
    , center : (Float, Float)
    , radius : Float}

-- Función para escribir un círculo en archivo
stringCircle : Circle -> String
stringCircle circle = Debug.toString circle.did ++ "," ++
                      Debug.toString (Tuple.first circle.center) ++ "," ++
                      Debug.toString (Tuple.second circle.center) ++ "," ++
                      Debug.toString circle.radius ++ "\n"

-- Tipo de dato linea
type alias Line = ((Float,Float),(Float,Float))


-- Función que dada una cadena, intenta traducirla a una línea
readLine : String -> Maybe Line
readLine line =
    case List.concat (List.map (String.split ",") (String.split "|" line)) of
        [x1,y1,x2,y2] -> String.toFloat x1 |> Maybe.andThen (\vx1 ->
                         String.toFloat y1 |> Maybe.andThen (\vy1 ->
                         String.toFloat x2 |> Maybe.andThen (\vx2 ->
                         String.toFloat y2 |> Maybe.andThen (\vy2 ->
                         Just ((vx1,vy1),(vx2,vy2))))))
        _ -> Nothing

-- Función que dada una línea la traduce a Svg
drawLine : Line -> Svg msg
drawLine ((x1,y1),(x2,y2)) = Svg.line [ A.x1 (String.fromFloat (x1))
                                      , A.y1 (String.fromFloat (-y1))
                                      , A.x2 (String.fromFloat (x2))
                                      , A.y2 (String.fromFloat (-y2))
                                      , A.stroke "Purple"
                                      , A.strokeWidth "1"
                                      , A.opacity "0.7"] [] 

-- Tipo de dato para modelar el estado de los círculos
type alias CircleState =
    { circles : List Circle
    , hull : List (Maybe Line)
    , current : Maybe Circle }

-- El estado incial    
initCircles =
    { circles = []
    , hull = []
    , current = Nothing }

-- Tipo de dato Modelo que almacena la información de la pantalla, del
-- ratón, y el estado de los círculos.
type alias Model =
    { screen : Screen
    , mouse : Mouse
    , circleState : CircleState}

-- Estado inicial del modelo
init () =
    ( { screen = toScreen 600 600
      , mouse = initMouse
      , circleState = initCircles }
    , Task.perform NewViewport Dom.getViewport)

-- Función update que recibe un mensaje y el modelo, y regresa la
-- actualización del modelo junto con un comando que se puede
-- ejecutar.
update msg model =
    case msg of
        NewViewport {viewport} ->
            ( act { model | screen = toScreen viewport.width viewport.height}
            , Cmd.none )
        Resize width height ->
            ( act { model | screen = toScreen (toFloat width) (toFloat height) }
            , Cmd.none )
        Save ->
            ( act model
            , Download.string "text.txt" "text/txt"
                ( String.join "" (List.map stringCircle model.circleState.circles) ) )
        Load ->
            ( act model
            , Select.file ["text/txt"] Received )
        Received file ->
            ( act model
            , Task.perform Decoded ( File.toString file ) )
        Decoded string ->
            ( act { model | circleState = { circles = model.circleState.circles
                                          , hull = List.map readLine (String.lines string)
                                          , current = model.circleState.current } }
            , Cmd.none )
        MouseMove x y ->
            ( act { model | mouse = { x = model.screen.left + x
                                    , y = model.screen.top - y
                                    , down = model.mouse.down } }
            , Cmd.none )
        MouseButton val ->
            ( act { model | mouse = { x = model.mouse.x
                                    , y = model.mouse.y
                                    , down = val } }
            , Cmd.none)

-- Función que dada una lista aumenta el id en uno dada una lista de
-- discos
assignId xs =
    case xs of
        [] -> 0
        x :: _ -> x.did + 1

-- Función que calcula la distancia entre dos puntos
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- Functión que actualiza el modelo, en este caso, indica si se ha
-- pulsado el ratón para dibujar o no un nuevo círculo
act model =
    let
        left = model.screen.left
        top = model.screen.top
        circles = model.circleState.circles
        current = model.circleState.current
        mouseDown = model.mouse.down
        x = model.mouse.x
        y = model.mouse.y
    in
        if x <= left + 100 && y >= top - 100 then
            model
        else
            case (circles, current) of
                (xs, Nothing) -> if mouseDown then
                                     { model | circleState =
                                                 { circles = xs
                                                 , hull = []
                                                 , current =
                                                     Just { did = assignId xs
                                                          , center = (x,y)
                                                          , radius = 0} } }
                                 else
                                     model
                (xs, Just disc) -> if mouseDown then
                                       { model | circleState =
                                                   { circles = xs
                                                   , hull = []
                                                   , current =
                                                       Just { disc | radius =
                                                                       distance disc.center (x,y) } } }
                                   else
                                       if disc.radius > 0 then
                                           { model | circleState = { circles = disc :: xs
                                                                   , hull = []
                                                                   , current = Nothing } }
                                       else
                                           { model | circleState = { circles = xs
                                                                   , hull = []
                                                                   , current = Nothing} }

-- Función que para observar cambios del navegador y regresa un Msg
-- que se puede procesar en la función update (puede afectar el modelo
-- si se desea, pero decidí no hacerlo)
subscriptions _ =
    Sub.batch
        [ E.onResize Resize
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False)) ]

-- Función que dado un modelo imprime en pantalla su estado
view model = { title = "Cierre Convexo"
             , body = render model ::
                      [Ht.div [H.style "position" "fixed"]
                           [ Ht.div [] [ Ht.button [He.onClick Save] [ text "Guardar" ] ]
                           , Ht.div [] [ Ht.button [He.onClick Load] [ text "Cargar" ] ]
                           , Ht.div [] [ Ht.text (String.fromFloat model.screen.width ) ] -- A partir de aquí, lo que sigue es para debuggear
                           , Ht.div [] [ Ht.text (String.fromFloat model.screen.height ) ]
                           , Ht.div [] [ Ht.text (String.fromFloat model.mouse.x) ]
                           , Ht.div [] [ Ht.text (String.fromFloat model.mouse.y) ]
                           , Ht.div [] [ Ht.text (if model.mouse.down then "Clicked" else "Unclicked")]]]}

-- Función que dibuja en Svg el modelo
render model =
    let
        w = String.fromFloat model.screen.width
        h = String.fromFloat model.screen.height
        x = String.fromFloat (model.screen.left)
        y = String.fromFloat model.screen.bottom
    in
        svg
        [ viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h)
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%" ]
        ((List.map P.renderShape (drawModel model)) ++ (drawHull model.circleState.hull))

-- Función para dibujar el cierre convexo en svg
drawHull list =
    case list of
        [] -> []
        (Just x) :: xs -> drawLine x :: drawHull xs
        Nothing :: xs -> drawHull xs

-- Función para dibujar los círculos (apoyado de la Paquetería
-- Playground (por flojera, la verdad))
drawModel model = [ (P.circle P.lightPurple 5
                    |> P.moveX model.mouse.x
                    |> P.moveY model.mouse.y) ] ++
                  (drawCircles model.circleState.circles) ++
                  (drawCircle model.circleState.current)

-- Función que traduce nuestros círculos a círculos de Playground
toPdotCircle circle = P.circle P.lightPurple circle.radius
                      |> P.moveX (Tuple.first circle.center)
                      |> P.moveY (Tuple.second circle.center)
                      |> P.fade 0.8

-- Función que dibuja el círculo que se está dibujando si es que
-- existe
drawCircle maybeCircle =
    case maybeCircle of
        Nothing -> []
        Just disc -> [ toPdotCircle disc |> P.fade 0.2 ]

-- Función que dibuja todos los círculos
drawCircles list = List.map toPdotCircle list
