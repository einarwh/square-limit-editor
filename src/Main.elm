module Main exposing (main)

import Browser
import Browser.Events exposing (onClick, onMouseMove, onMouseDown)
import Html exposing (Html, text, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode

type alias Pos = { x : Float, y : Float }

type DragBezierPoint = Moving Pos | Fixed Pos

type alias BezierLine = { start : Pos, cp1 : Pos, cp2 : Pos, end : Pos }

type alias DragBezierLine = { start : DragBezierPoint, cp1 : DragBezierPoint, cp2 : DragBezierPoint, end : DragBezierPoint }

type Model = Dragging { line : DragBezierLine, lines : List BezierLine, message : String }
             | Drawing { startPos: Pos, currentPos: Pos, lines : List BezierLine, message : String }
             | Default { lines: List BezierLine, message : String }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( Default { message = "Click on the page", lines = [] }, Cmd.none )


type Msg = LeftClick Pos
           | RightClick Pos
           | MouseMove Pos
           | MouseDown Pos

createBezierLine : Pos -> Pos -> BezierLine
createBezierLine start end = 
    let dx = (end.x - start.x) / 3.0 
        dy = (end.y - start.y) / 3.0
    in 
        { start = start 
        , cp1 = { x = start.x + dx, y = start.y + dy } 
        , cp2 = { x = start.x + 2 * dx, y = start.y + 2 * dy } 
        , end = end
        }

distanceBetweenPoints: Pos -> Pos -> Float 
distanceBetweenPoints pos1 pos2 = 
    let
        dx = pos1.x - pos2.x 
        dy = pos1.y - pos2.y 
    in 
        (dx * dx + dy * dy) |> sqrt

closeEnough: Pos -> Pos -> Bool
closeEnough pos1 pos2 = 
    distanceBetweenPoints pos1 pos2 < 2 

findPointInBezier : Pos -> BezierLine -> Maybe DragBezierLine 
findPointInBezier pos bezier = 
    if closeEnough pos bezier.start then 
        Just { start = Moving bezier.start, cp1 = Fixed bezier.cp1, cp2 = Fixed bezier.cp2, end = Fixed bezier.end }
    else if closeEnough pos bezier.cp1 then 
        Just { start = Fixed bezier.start, cp1 = Moving bezier.cp1, cp2 = Fixed bezier.cp2, end = Fixed bezier.end }
    else if closeEnough pos bezier.cp2 then 
        Just { start = Fixed bezier.start, cp1 = Fixed bezier.cp1, cp2 = Moving bezier.cp2, end = Fixed bezier.end }
    else if closeEnough pos bezier.end then 
        Just { start = Fixed bezier.start, cp1 = Fixed bezier.cp1, cp2 = Fixed bezier.cp2, end = Moving bezier.end }
    else 
        Nothing

checkGrab : Pos -> List BezierLine -> (Maybe DragBezierLine, List BezierLine)
checkGrab pos beziers = 
    case beziers of 
        [] -> (Nothing, [])
        h :: t -> 
            case findPointInBezier pos h of 
                Just dragBezier -> (Just dragBezier, t)
                Nothing -> 
                    case checkGrab pos t of 
                        (Nothing, _) -> (Nothing, beziers)
                        (Just x, normalLines) -> (Just x, h :: normalLines)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of 
        Dragging _ -> 
            (model, Cmd.none)
        Drawing drawState -> 
            case msg of 
                LeftClick pos -> 
                    -- End line now and add to lines.
                    let 
                        line = createBezierLine drawState.startPos pos 
                    in 
                        ( Default { message = "Complete line"
                                  , lines = line :: drawState.lines
                                  }
                        , Cmd.none
                        )
                    -- Outline 
                MouseMove pos -> 
                    (Drawing { drawState | currentPos = pos }, Cmd.none)
                _ -> (model, Cmd.none)
        Default {lines, message} ->
            -- Not currently doing anything! 
            case msg of 
                LeftClick pos ->
                    -- Start drawing.
                    let newModel = Drawing { message = "Start drawing... "
                                           , lines = lines
                                           , startPos = pos
                                           , currentPos = pos}
                    in 
                        (newModel, Cmd.none)
                MouseDown pos -> 
                    -- Check to see if we click down to grab anything?
                    -- Check each point of each line.
                    -- Drag StartPoint | ControlPoint1 | ControlPoint2 | EndPoint

                    let 
                        newModel2 = Default { message = "Grabbing?"
                                            , lines = lines }
                    in 
                        case checkGrab pos lines of 
                            (Just dragBezier, normalLines) -> 
                                let
                                    newModel = Dragging { message = "Grabbing!"
                                                        , lines = normalLines
                                                        , line = dragBezier }
                                in
                                    (newModel, Cmd.none)
                            (Nothing, _) -> (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)

createSvgLine : BezierLine -> List (Svg Msg)
createSvgLine line = 
    let x1Str = String.fromFloat line.start.x 
        y1Str = String.fromFloat line.start.y 
        x2Str = String.fromFloat line.end.x 
        y2Str = String.fromFloat line.end.y
        xCp1Str = String.fromFloat line.cp1.x
        yCp1Str = String.fromFloat line.cp1.y
        xCp2Str = String.fromFloat line.cp2.x
        yCp2Str = String.fromFloat line.cp2.y
    in  
        [ Svg.line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "black" ] []
        , Svg.circle [ cx x1Str, cy y1Str, r "3", stroke "blue", fill "none" ] []
        , Svg.circle [ cx x2Str, cy y2Str, r "3", stroke "blue", fill "none" ] []
        , Svg.circle [ cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill "none" ] []
        , Svg.circle [ cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill "none" ] []
        ]

createCurrentDrawingLine : Pos -> Pos -> Svg Msg
createCurrentDrawingLine startPos endPos = 
    let x1Str = String.fromFloat startPos.x 
        y1Str = String.fromFloat startPos.y 
        x2Str = String.fromFloat endPos.x 
        y2Str = String.fromFloat endPos.y
    in  
        Svg.line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "purple", strokeDasharray "1" ] []

getPositionOfDragPoint : DragBezierPoint -> Pos 
getPositionOfDragPoint dragPoint = 
    case dragPoint of 
        Fixed pt -> pt 
        Moving pt -> pt

getFillOfDragPoint : DragBezierPoint -> String -> String 
getFillOfDragPoint dragPoint color = 
    case dragPoint of 
        Fixed _ -> "none"
        Moving _ -> color


createDragSvgLine : DragBezierLine -> List (Svg Msg)
createDragSvgLine line = 
    let 
        x1Str = String.fromFloat (getPositionOfDragPoint line.start).x 
        y1Str = String.fromFloat (getPositionOfDragPoint line.start).y 
        x2Str = String.fromFloat (getPositionOfDragPoint line.end).x 
        y2Str = String.fromFloat (getPositionOfDragPoint line.end).y
        xCp1Str = String.fromFloat (getPositionOfDragPoint line.cp1).x
        yCp1Str = String.fromFloat (getPositionOfDragPoint line.cp1).y
        xCp2Str = String.fromFloat (getPositionOfDragPoint line.cp2).x
        yCp2Str = String.fromFloat (getPositionOfDragPoint line.cp2).y
    in  
        [ Svg.line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "black" ] []
        , Svg.circle [ cx x1Str, cy y1Str, r "3", stroke "purple", fill (getFillOfDragPoint line.start "purple") ] []
        , Svg.circle [ cx x2Str, cy y2Str, r "3", stroke "blue", fill (getFillOfDragPoint line.end "blue") ] []
        , Svg.circle [ cx xCp1Str, cy yCp1Str, r "3", stroke "green", fill (getFillOfDragPoint line.cp1 "green") ] []
        , Svg.circle [ cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill (getFillOfDragPoint line.cp2 "red") ] []
        ]            

getSvgElements : Model -> List (Svg Msg)
getSvgElements model = 
    case model of 
        Drawing { startPos, currentPos, lines } -> 
            let 
                svgLines = List.concatMap createSvgLine lines
                currentLine = createCurrentDrawingLine startPos currentPos 
            in 
                currentLine :: svgLines
        Dragging { lines, line } -> 
            let 
                svgLines = List.concatMap createSvgLine lines
                dragLine = createDragSvgLine line 
            in 
                dragLine ++ svgLines
        Default { lines } -> 
            let
                svgLines = List.concatMap createSvgLine lines
            in        
                svgLines

getMessageElement : Model -> Html Msg 
getMessageElement model = 
    case model of 
        Drawing { message } -> 
            Html.text ("Drawing..." ++ message)
        Dragging { message } -> 
            Html.text ("Dragging..." ++ message)
        Default { message } -> 
            Html.text ("Default..." ++ message)

view : Model -> Html Msg
view model =
    let
        svgElements = getSvgElements model
        msgElement = getMessageElement model
    in
        div [] 
        [
          div [] [ msgElement]
        , div []
              [ svg
                    [ width "500"
                    , height "500"
                    , viewBox "0 0 500 500"
                    ]
                    svgElements
              ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let 
        offsetX = Decode.field "offsetX" Decode.float
        offsetY = Decode.field "offsetY" Decode.float
    in 
        case model of 
            Drawing _ -> 
                Sub.batch 
                [ onClick (Decode.map2 (\a -> \b -> LeftClick { x = a, y = b }) offsetX offsetY)
                , onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY) ]
            Dragging _ -> 
                Sub.batch []
            Default _ -> 
                Sub.batch 
                [ onClick (Decode.map2 (\a -> \b -> LeftClick { x = a, y = b }) offsetX offsetY)
                , onMouseDown (Decode.map2 (\a -> \b -> MouseDown { x = a, y = b }) offsetX offsetY) ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
