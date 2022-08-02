module Main exposing (main)

import Browser
import Browser.Events exposing (onClick, onMouseMove, onMouseDown, onMouseUp)
import Html exposing (Html, text, div, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Json.Decode as Decode

type alias Pos = { x : Float, y : Float }

type alias LineId = String

type alias BezierLine = { id : LineId, start : Pos, cp1 : Pos, cp2 : Pos, end : Pos }

--type alias Line = { lineNo : Int, start : Pos }

type Action = AddLine BezierLine
              | EditLine BezierLine
              | DeleteLine LineId

type LineStatus = NormalLine | SelectedLine | HoverLine

type DragBezierPoint = MovingPoint Pos | FixedPoint Pos


type alias DragBezierLine = { id : LineId, start : DragBezierPoint, cp1 : DragBezierPoint, cp2 : DragBezierPoint, end : DragBezierPoint }

type DraggingBezierLine = FixedLine BezierLine | ChangingLine DragBezierLine 

-- Model 
--   Reflect? Y/N
--   Default
--     Select element -> Selected element
--     Start drawing (Left click) => Drawing
--   Drawing 
--     End drawing (Left click) => Default
--   Selected element
--     Delete element => Default
--     Unselect element => Default
--     Drag points on element => Dragging (still with selected element implicit)
--   Dragging (mouse button is down) 
--     Stop dragging (MouseUp) => Selected
-- Render : Simple | T-tile | Square Limit

type Editor = 
  Default 
  | Drawing { startPos : Pos }
  | Selected { id : LineId }
  | Hovering { id : LineId }

--  Dragging { dragLines : List DraggingBezierLine, message : String }
--              | Dragged { lines : List BezierLine, message : String }
--              | Drawing { startPos: Pos, currentPos: Pos, lines : List BezierLine, message : String }
--              | Default { lines: List BezierLine, message : String }
--              | Selected { lines : List BezierLine, message : String}

type Render = Simple | Tile | Limit

type alias Model = 
  { editor : Editor
  , status : String 
  , currentLineNo : Int
  , pos : Pos
  , history : List Action
  , reflect : Bool
  , render : Render }

init : flags -> ( Model, Cmd Msg )
init flags =
    let
        editor = Default 
        model = 
          { editor = editor
          , status = ""
          , currentLineNo = 0
          , pos = { x = 0, y = 0 }
          , history = []
          , reflect = False
          , render = Simple}
        cmd = Cmd.none 
    in
        (model, cmd)

type Msg = LeftClick Pos
           | RightClick Pos
           | JustClick 
           | MouseMove Pos
           | MouseDown Pos
           | MouseUp Pos
           | SelectElement LineId
           | UnselectElement LineId
           | HoverElement LineId
           | UnhoverElement LineId
           | DeleteSelected
           | UndoAction

onSvgClick : msg -> Attribute msg
onSvgClick msg =
  Svg.Events.stopPropagationOn "click" (Decode.map (\m-> (m, True)) (Decode.succeed msg))

createBezierLine : LineId -> Pos -> Pos -> BezierLine
createBezierLine id start end = 
    let dx = (end.x - start.x) / 3.0 
        dy = (end.y - start.y) / 3.0
    in 
        { id = id
        , start = start 
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
        Just { id = bezier.id
             , start = MovingPoint bezier.start
             , cp1 = FixedPoint bezier.cp1
             , cp2 = FixedPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.cp1 then 
        Just { id = bezier.id
             , start = FixedPoint bezier.start
             , cp1 = MovingPoint bezier.cp1
             , cp2 = FixedPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.cp2 then 
        Just { id = bezier.id
             , start = FixedPoint bezier.start
             , cp1 = FixedPoint bezier.cp1
             , cp2 = MovingPoint bezier.cp2
             , end = FixedPoint bezier.end }
    else if closeEnough pos bezier.end then 
        Just { id = bezier.id
             , start = FixedPoint bezier.start
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
            { id = "."
            , start = getPositionOfDragPoint changing.start 
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
    { id = dragLine.id
    , start = updateDragPoint pos dragLine.start
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
        { editor, currentLineNo, reflect, render, history, pos } -> 
            case editor of
                    Selected { id } -> 
                        case msg of
                            DeleteSelected -> 
                                let 
                                    act = DeleteLine id
                                    ed = Default 
                                in 
                                    ({model | editor = ed, history = act :: history}, Cmd.none)

                            UnselectElement clickId -> 
                                let ed = Default 
                                in 
                                    ({ model | editor = ed, status = "select..."}, Cmd.none) 
                            -- JustClick -> 
                            --     ({ model | editor = Default, status = "wel?"}, Cmd.none) 
                            _ -> ({ model | status = "selected" }, Cmd.none)
                    Hovering _ -> 
                        case msg of 
                            SelectElement clickId -> 
                                let ed = Selected { id = clickId }
                                in 
                                    ({ model | editor = ed, status = "select..."}, Cmd.none) 
                            UnhoverElement _ -> 
                                ({ model | status = "hovering", editor = Default }, Cmd.none)     
                            _ -> (model, Cmd.none)     
                    Drawing { startPos } -> 
                        case msg of 
                            JustClick -> 
                                -- End line now and add to lines.
                                let 
                                    id = "path#" ++ String.fromInt currentLineNo
                                    line = createBezierLine id startPos pos 
                                    act = AddLine line
                                    ed = Default 
                                in 
                                    ({model | editor = ed, currentLineNo = currentLineNo + 1, history = act :: history}, Cmd.none)
                                -- Outline 
                            MouseMove newPos -> 
                                ({ model | pos = newPos }, Cmd.none)
                            _ -> (model, Cmd.none)
                    Default ->
                        -- Not currently doing anything! 
                        case msg of 
                            UndoAction -> 
                                let
                                    hist = case history of [] -> [] 
                                                           h :: t -> t
                                in
                                    ({model | history = hist }, Cmd.none)
                            JustClick -> 
                                let ed = Drawing { startPos = pos}
                                in 
                                    ({ model | editor = ed}, Cmd.none) 
                            MouseMove newPos -> 
                                let ed = Default
                                in 
                                    ({ model | editor = ed, pos = newPos }, Cmd.none)
                            SelectElement id -> 
                                let ed = Selected { id = id}
                                in 
                                    ({ model | editor = ed}, Cmd.none) 
                            HoverElement id -> 
                                let ed = Hovering { id = id}
                                in 
                                    ({ model | editor = ed}, Cmd.none) 
                            _ -> 
                                (model, Cmd.none)

createSvgLine : LineStatus -> BezierLine -> List (Svg Msg)
createSvgLine status line = 
    let 
        pathId = line.id
        x1Str = String.fromFloat line.start.x 
        y1Str = String.fromFloat line.start.y 
        x2Str = String.fromFloat line.end.x 
        y2Str = String.fromFloat line.end.y
        xCp1Str = String.fromFloat line.cp1.x
        yCp1Str = String.fromFloat line.cp1.y
        xCp2Str = String.fromFloat line.cp2.x
        yCp2Str = String.fromFloat line.cp2.y
        dStr = "M" ++ x1Str ++ " " ++ y1Str ++ " C " ++ xCp1Str ++ " " ++ yCp1Str ++ ", " ++ xCp2Str ++ " " ++ yCp2Str ++ ", " ++ x2Str ++ " " ++ y2Str
    in  
        case status of 
            NormalLine -> 
               [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (SelectElement pathId ), Svg.Events.onMouseOver (HoverElement pathId )  ] [] ] 
            SelectedLine -> 
                [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (UnselectElement pathId) ] []
                --[ Svg.path [ d dStr, stroke "black", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-sp"), cx x1Str, cy y1Str, r "3", stroke "blue", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-ep"), cx x2Str, cy y2Str, r "3", stroke "blue", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-cp1"), cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-cp2"), cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill "none" ] []
                ]
            HoverLine -> 
                [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (SelectElement pathId), Svg.Events.onMouseOut (UnhoverElement pathId) ] []
                --[ Svg.path [ d dStr, stroke "black", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-sp"), cx x1Str, cy y1Str, r "3", stroke "blue", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-ep"), cx x2Str, cy y2Str, r "3", stroke "blue", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-cp1"), cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill "none" ] []
                , Svg.circle [ id (pathId ++ "-cp2"), cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill "none" ] []
                ]

createCurrentDrawingLine : Pos -> Pos -> Svg Msg
createCurrentDrawingLine startPos endPos = 
    let x1Str = String.fromFloat startPos.x 
        y1Str = String.fromFloat startPos.y 
        x2Str = String.fromFloat endPos.x 
        y2Str = String.fromFloat endPos.y
    in  
        Svg.line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "purple", strokeDasharray "1" ] []

createTriangleLine : Pos -> Pos -> Svg Msg
createTriangleLine startPos endPos = 
    let x1Str = String.fromFloat startPos.x 
        y1Str = String.fromFloat startPos.y 
        x2Str = String.fromFloat endPos.x 
        y2Str = String.fromFloat endPos.y
    in  
        Svg.line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "lightgrey", strokeDasharray "1" ] []

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
        pathId = line.id

        x1Str = String.fromFloat (getPositionOfDragPoint line.start).x 
        y1Str = String.fromFloat (getPositionOfDragPoint line.start).y 
        x2Str = String.fromFloat (getPositionOfDragPoint line.end).x 
        y2Str = String.fromFloat (getPositionOfDragPoint line.end).y
        xCp1Str = String.fromFloat (getPositionOfDragPoint line.cp1).x
        yCp1Str = String.fromFloat (getPositionOfDragPoint line.cp1).y
        xCp2Str = String.fromFloat (getPositionOfDragPoint line.cp2).x
        yCp2Str = String.fromFloat (getPositionOfDragPoint line.cp2).y
        dStr = "M" ++ x1Str ++ " " ++ y1Str ++ " C " ++ xCp1Str ++ " " ++ yCp1Str ++ ", " ++ xCp2Str ++ " " ++ yCp2Str ++ ", " ++ x2Str ++ " " ++ y2Str
    in  
        [ Svg.path [ id pathId, d dStr, stroke "black", fill "none" ] []
        , Svg.circle [ cx x1Str, cy y1Str, r "3", stroke "blue", fill (getFillOfDragPoint line.start "blue") ] []
        , Svg.circle [ cx x2Str, cy y2Str, r "3", stroke "blue", fill (getFillOfDragPoint line.end "blue") ] []
        , Svg.circle [ cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill (getFillOfDragPoint line.cp1 "red") ] []
        , Svg.circle [ cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill (getFillOfDragPoint line.cp2 "red") ] []
        ]            

createDraggingSvgLine : DraggingBezierLine -> List (Svg Msg)
createDraggingSvgLine line = 
    case line of 
        ChangingLine changing -> createDragSvgLine changing
        FixedLine fixed -> createSvgLine NormalLine fixed

toSvgLines : List BezierLine -> List (Svg Msg)
toSvgLines lines = 
    lines |> List.concatMap (createSvgLine NormalLine)

tryFind : (BezierLine -> Bool) -> List BezierLine -> Maybe BezierLine
tryFind pred list = 
    case list of 
        [] -> Nothing 
        h :: t -> 
            if pred h then Just h 
            else tryFind pred t 

toSvgLinesWithSelection : LineId -> List BezierLine -> List (Svg Msg)
toSvgLinesWithSelection id lines = 
    case tryFind (\line -> line.id == id) lines of 
        Nothing -> toSvgLines lines 
        Just selection -> 
            let
                rest = List.filter (\line -> line.id /= id) lines
                selected = createSvgLine SelectedLine selection
            in
                selected ++ toSvgLines rest  
            
toSvgLinesWithHover : LineId -> List BezierLine -> List (Svg Msg)
toSvgLinesWithHover id lines = 
    case tryFind (\line -> line.id == id) lines of 
        Nothing -> toSvgLines lines 
        Just selection -> 
            let
                rest = List.filter (\line -> line.id /= id) lines
                selected = createSvgLine HoverLine selection
            in
                selected ++ toSvgLines rest  

processAction : Action -> List BezierLine -> List BezierLine
processAction act lines = 
    case act of 
        AddLine bezier -> bezier :: lines 
        EditLine bezier -> 
            List.map (\line -> if line.id == bezier.id then bezier else line) lines 
        DeleteLine id -> 
            List.filter (\line -> line.id /= id) lines 

getLinesFromHistory : List Action -> List BezierLine
getLinesFromHistory history = 
    List.foldr processAction [] history

getSvgElements : Model -> List (Svg Msg)
getSvgElements model = 
    case model of 
        { editor, reflect, render, history, pos } ->
            case editor of 
                Drawing { startPos } -> 
                    let 
                        lines = getLinesFromHistory history
                        svgLines = toSvgLines lines
                        currentLine = createCurrentDrawingLine startPos pos 
                    in 
                        currentLine :: svgLines
                Default -> 
                    let
                        lines = getLinesFromHistory history
                        svgLines = toSvgLines lines
                    in        
                        svgLines
                Hovering { id } -> 
                    let
                        lines = getLinesFromHistory history
                        svgLines = toSvgLinesWithHover id lines
                    in        
                        svgLines
                Selected { id } -> 
                    let
                        lines = getLinesFromHistory history
                        svgLines = toSvgLinesWithSelection id lines
                    in        
                        svgLines

getMessageElement : Model -> Html Msg 
getMessageElement model = 
    case model of 
        { editor, reflect, render } ->
            case editor of 
                Drawing _-> 
                    Html.text ("Drawing..." ++ model.status)
                Default -> 
                    Html.text ("Default..." ++ model.status)
                Selected _ -> 
                    Html.text ("Selected..." ++ model.status)
                Hovering _ -> 
                    Html.text ("Hovering..." ++ model.status)

view : Model -> Html Msg
view model =
    let
        svgElements = getSvgElements model
        msgElement = getMessageElement model
        triangleElements = 
          [ createTriangleLine { x = 100.0, y = 300.0 } { x = 100.0, y = 100.0 }
          , createTriangleLine { x = 100.0, y = 300.0 } { x = 300.0, y = 300.0 }
          , createTriangleLine { x = 100.0, y = 100.0 } { x = 300.0, y = 300.0 }
          ]
    in
        div [] 
        [
          div [] [ msgElement ]
        , table 
            [] 
            [ tr 
                [] 
                [ td 
                    []
                    [ table 
                        [ Html.Attributes.style "background-color" "pink" ] 
                        [ tr 
                            []
                            [ td 
                                [] 
                                [ 
                                  Html.input [ id "reflect", type_ "checkbox" ] []
                                , Html.label [ Html.Attributes.for "reflect" ] [ Html.text "Reflect" ]
                                , Html.button [ Html.Events.onClick UndoAction ] [ Html.text "Undo" ]
                                , Html.button [ Html.Events.onClick DeleteSelected ] [ Html.text "Delete" ] ] ] 
                        , tr 
                            [] 
                            [ td 
                                [] 
                                [ svg
                                    [ width "400"
                                    , height "400"
                                    , viewBox "0 0 400 400"
                                    , Html.Attributes.style "background-color" "white"
                                    , Svg.Events.onClick JustClick
                                    ]
                                    (svgElements ++ triangleElements) ] ] ] ] 
                , td 
                    []
                    [ table 
                        [ Html.Attributes.style "background-color" "green" ] 
                        [ tr 
                            [] 
                            [ Html.button [] [ Html.text "Simple" ]
                            , Html.button [] [ Html.text "T-tile" ]
                            , Html.button [] [ Html.text "Limit" ] ]
                        , tr 
                            [] 
                            [ td 
                                [] 
                                [ svg
                                    [ width "400"
                                    , height "400"
                                    , viewBox "0 0 400 400"
                                    , Html.Attributes.style "background-color" "yellow"
                                    ]
                                    svgElements] ] ] ] ]
                    ] ] 

subscriptions : Model -> Sub Msg
subscriptions {editor, reflect, render} =
    let 
        offsetX = Decode.field "offsetX" Decode.float
        offsetY = Decode.field "offsetY" Decode.float
    in 
        onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY)
        -- case editor of 
        --     Drawing _ -> 
        --         Sub.batch 
        --         [ onClick (Decode.map2 (\a -> \b -> LeftClick { x = a, y = b }) offsetX offsetY)
        --         , onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY) ]
        --     Dragging _ -> 
        --         Sub.batch
        --         [ onMouseUp (Decode.map2 (\a -> \b -> MouseUp { x = a, y = b }) offsetX offsetY)
        --         , onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY) ]
        --     Dragged _ -> 
        --         Sub.batch
        --         [ onClick (Decode.map2 (\a -> \b -> MouseUp { x = a, y = b }) offsetX offsetY) ]
        --     Default _ -> 
        --         Sub.batch 
        --         [ --onClick (Decode.map2 (\a -> \b -> LeftClick { x = a, y = b }) offsetX offsetY)
        --           onMouseDown (Decode.map2 (\a -> \b -> MouseDown { x = a, y = b }) offsetX offsetY)
        --         , on ]
        --     Selected _ -> Sub.none

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
