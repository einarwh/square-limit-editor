module Main exposing (main)

import Browser
import Browser.Events exposing (onClick, onMouseMove, onMouseDown, onMouseUp)
import Html exposing (Html, text, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode

type alias Pos = { x : Float, y : Float }

type DragBezierPoint = MovingPoint Pos | FixedPoint Pos

type alias BezierLine = { start : Pos, cp1 : Pos, cp2 : Pos, end : Pos }

type alias DragBezierLine = { start : DragBezierPoint, cp1 : DragBezierPoint, cp2 : DragBezierPoint, end : DragBezierPoint }

type DraggingBezierLine = FixedLine BezierLine | ChangingLine DragBezierLine

type Model = Dragging { dragLines : List DraggingBezierLine, message : String }
             | Dragged { lines : List BezierLine, message : String }
             | Drawing { startPos: Pos, currentPos: Pos, lines : List BezierLine, message : String }
             | Default { lines: List BezierLine, message : String }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( Default { message = "Click on the page", lines = [] }, Cmd.none )


type Msg = LeftClick Pos
           | RightClick Pos
           | MouseMove Pos
           | MouseDown Pos
           | MouseUp Pos

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
    distanceBetweenPoints pos1 pos2 < 3 

findPointInBezier : Pos -> BezierLine -> Maybe DragBezierLine 
findPointInBezier pos bezier = 
    if closeEnough pos bezier.start then 
        Just { start = MovingPoint bezier.start
             , cp1 = FixedPoint bezier.cp1
             , cp2 = FixedPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.cp1 then 
        Just { start = FixedPoint bezier.start
             , cp1 = MovingPoint bezier.cp1
             , cp2 = FixedPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.cp2 then 
        Just { start = FixedPoint bezier.start
             , cp1 = FixedPoint bezier.cp1
             , cp2 = MovingPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.end then 
        Just { start = FixedPoint bezier.start
             , cp1 = FixedPoint bezier.cp1
             , cp2 = FixedPoint bezier.cp2
             , end = MovingPoint bezier.end }
    else 
        Nothing

checkGrab : Pos -> List BezierLine -> Bool -> List DraggingBezierLine
checkGrab pos beziers found = 
    case beziers of 
        [] -> []
        h :: t ->
            if found then 
                FixedLine h :: checkGrab pos t True 
            else 
                case findPointInBezier pos h of 
                    Just dragBezier -> 
                        ChangingLine dragBezier :: checkGrab pos t True 
                    Nothing -> 
                        FixedLine h :: checkGrab pos t False 

grabbed : List DraggingBezierLine -> Bool
grabbed dragBeziers = 
    List.any (\bz -> case bz of ChangingLine _ -> True 
                                FixedLine _ -> False) dragBeziers

toNormalLine : DraggingBezierLine -> BezierLine 
toNormalLine dragLine = 
    case dragLine of
        FixedLine line -> line 
        ChangingLine changing ->  
            { start = getPositionOfDragPoint changing.start 
            , cp1 = getPositionOfDragPoint changing.cp1
            , cp2 = getPositionOfDragPoint changing.cp2 
            , end = getPositionOfDragPoint changing.end }

toNormalLines : List DraggingBezierLine -> List BezierLine
toNormalLines dragLines = 
    List.map toNormalLine dragLines

updateDragPoint : Pos -> DragBezierPoint -> DragBezierPoint 
updateDragPoint pos point =
    case point of 
        FixedPoint fixed -> FixedPoint fixed 
        MovingPoint moving -> MovingPoint pos

updateDragLine : Pos -> DragBezierLine -> DragBezierLine
updateDragLine pos dragLine = 
    { start = updateDragPoint pos dragLine.start
    , cp1 = updateDragPoint pos dragLine.cp1 
    , cp2 = updateDragPoint pos dragLine.cp2 
    , end = updateDragPoint pos dragLine.end }

updateDraggingLine : Pos -> DraggingBezierLine -> DraggingBezierLine
updateDraggingLine pos draggingLine = 
    case draggingLine of 
        FixedLine line -> FixedLine line 
        ChangingLine changing -> ChangingLine (updateDragLine pos changing)

updateDraggingLines : Pos -> List DraggingBezierLine -> List DraggingBezierLine
updateDraggingLines pos draggingLines = 
    List.map (updateDraggingLine pos) draggingLines

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of 
        Dragging { dragLines, message } -> 
            case msg of 
                MouseMove pos -> 
                    (Dragging { dragLines = updateDraggingLines pos dragLines, message = message }, Cmd.none)
                MouseUp pos -> 
                    let
                        lines = toNormalLines dragLines
                    in
                    
                    (Dragged { lines = lines, message = "Dragged" }, Cmd.none)
                _ -> (model, Cmd.none)
        Dragged { lines, message } -> 
            case msg of 
                _ -> 
                   (Default { lines = lines, message = "Default" }, Cmd.none)
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
                        dragLines = checkGrab pos lines False
                    in
                        if grabbed dragLines then 
                            let
                                newModel = Dragging { message = "Dragging!"
                                                    , dragLines = dragLines }
                            in
                                (newModel, Cmd.none)
                        else (model, Cmd.none)                    
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
        FixedPoint pt -> pt 
        MovingPoint pt -> pt

getFillOfDragPoint : DragBezierPoint -> String -> String 
getFillOfDragPoint dragPoint color = 
    case dragPoint of 
        FixedPoint _ -> "none"
        MovingPoint _ -> color

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

createDraggingSvgLine : DraggingBezierLine -> List (Svg Msg)
createDraggingSvgLine line = 
    case line of 
        ChangingLine changing -> createDragSvgLine changing
        FixedLine fixed -> createSvgLine fixed

getSvgElements : Model -> List (Svg Msg)
getSvgElements model = 
    case model of 
        Drawing { startPos, currentPos, lines } -> 
            let 
                svgLines = List.concatMap createSvgLine lines
                currentLine = createCurrentDrawingLine startPos currentPos 
            in 
                currentLine :: svgLines
        Dragging { dragLines } -> 
            let 
                svgLines = List.concatMap createDraggingSvgLine dragLines
            in 
                svgLines
        Dragged { lines } -> 
            let
                svgLines = List.concatMap createSvgLine lines
            in        
                svgLines
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
        Dragged { message } -> 
            Html.text ("Dragged..." ++ message)
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
                Sub.batch
                [ onMouseUp (Decode.map2 (\a -> \b -> MouseUp { x = a, y = b }) offsetX offsetY)
                , onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY) ]
            Dragged _ -> 
                Sub.batch
                [ onClick (Decode.map2 (\a -> \b -> MouseUp { x = a, y = b }) offsetX offsetY) ]
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
