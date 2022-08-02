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

type LinePoint = StartPoint 
                 | ControlPoint1 
                 | ControlPoint2 
                 | EndPoint

type Action = AddLine BezierLine
              | EditLine LineId LinePoint Pos
              | DeleteLine LineId

type LineStatus = NormalLine | SelectedLine | HoverLine

type Editor = 
  Default 
  | Drawing { startPos : Pos }
  | Selected { id : LineId }
  | Hovering { id : LineId }
  | Dragging { id: LineId, point: LinePoint }
  | Stopped { id : LineId }

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

type Msg = JustClick 
           | MouseMove Pos
           | MouseUp Pos
           | SelectElement LineId
           | UnselectElement LineId
           | HoverElement LineId
           | UnhoverElement LineId
           | DeleteSelected
           | UndoAction
           | StartDragLinePoint LineId LinePoint
           | StopDragLinePoint LineId LinePoint

onSvgClick : msg -> Attribute msg
onSvgClick msg =
  Svg.Events.stopPropagationOn "click" (Decode.map (\m-> (m, True)) (Decode.succeed msg))

onSvgMouseDown : msg -> Attribute msg
onSvgMouseDown msg =
  Svg.Events.stopPropagationOn "mousedown" (Decode.map (\m-> (m, True)) (Decode.succeed msg))

onSvgMouseUp : msg -> Attribute msg
onSvgMouseUp msg =
  Svg.Events.stopPropagationOn "mouseup" (Decode.map (\m-> (m, True)) (Decode.succeed msg))

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of 
        { editor, currentLineNo, reflect, render, history, pos } -> 
            case editor of
                    Stopped { id } -> 
                        case msg of 
                            JustClick -> 
                                ({ model | editor = Selected {id=id} }, Cmd.none)
                            MouseMove newPos -> 
                                ({ model | pos = newPos }, Cmd.none)
                            _ ->
                                ({ model | editor = Selected {id=id} }, Cmd.none)
                    Selected { id } -> 
                        case msg of
                            UndoAction -> 
                                let
                                    hist = case history of [] -> [] 
                                                           _ :: t -> t
                                in
                                    ({model | history = hist }, Cmd.none)

                            DeleteSelected -> 
                                let 
                                    act = DeleteLine id
                                    ed = Default 
                                in 
                                    ({model | editor = ed, history = act :: history}, Cmd.none)
                            HoverElement hoverId -> 
                                let ed = Hovering { id = hoverId }
                                in 
                                    ({ model | editor = ed}, Cmd.none) 

                            UnselectElement clickId -> 
                                let ed = Default 
                                in 
                                    ({ model | editor = ed, status = "select..."}, Cmd.none) 
                            StartDragLinePoint lineId point -> 
                                ({ model | editor = Dragging { id = lineId, point = point }, status = "select..."}, Cmd.none) 

                            MouseMove newPos -> 
                                ({ model | pos = newPos }, Cmd.none)

                            JustClick  -> 
                                ({ model | editor = Default }, Cmd.none)
                            _ -> ({ model | status = "..." }, Cmd.none)
                    Hovering _ -> 
                        case msg of 
                            MouseMove newPos -> 
                                ({ model | pos = newPos }, Cmd.none)
                            SelectElement clickId -> 
                                let ed = Selected { id = clickId }
                                in 
                                    ({ model | editor = ed, status = "select..."}, Cmd.none) 
                            UnhoverElement _ -> 
                                ({ model | status = "hovering", editor = Default }, Cmd.none)
                            StartDragLinePoint lineId point -> 
                                ({ model | editor = Dragging { id = lineId, point = point }, status = "select..."}, Cmd.none) 
                            JustClick -> 
                                ({model | editor = Default} , Cmd.none)     
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
                    Dragging { id, point } -> 
                        case msg of 
                            MouseMove newPos -> 
                                ({ model | pos = newPos, status = "dragging..." }, Cmd.none)
                            StopDragLinePoint lineId pt -> 
                                let
                                    act = EditLine lineId pt pos
                                in
                                    ({ model | editor = Stopped { id = id }, history = act :: history, status = "stopped..." }, Cmd.none) 
                            MouseUp pt -> 
                                ({ model | pos = pt, editor = Selected { id = id }, status = "mouseup..."}, Cmd.none) 
                            _ -> ({ model | status = "somethig?" }, Cmd.none)
                    Default ->
                        -- Not currently doing anything! 
                        case msg of 
                            UndoAction -> 
                                let
                                    hist = case history of [] -> [] 
                                                           _ :: t -> t
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
               [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (SelectElement pathId ), Svg.Events.onMouseOver (HoverElement pathId) ] [] ] 
            SelectedLine -> 
                [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (UnselectElement pathId) ] []
                , Svg.circle [ id (pathId ++ "-sp"), cx x1Str, cy y1Str, r "3", stroke "blue", fill "blue", onSvgMouseDown (StartDragLinePoint pathId StartPoint), onSvgMouseUp (StopDragLinePoint pathId StartPoint) ] []
                , Svg.circle [ id (pathId ++ "-ep"), cx x2Str, cy y2Str, r "3", stroke "blue", fill "blue", onSvgMouseDown (StartDragLinePoint pathId EndPoint), onSvgMouseUp (StopDragLinePoint pathId EndPoint) ] []
                , Svg.circle [ id (pathId ++ "-cp1"), cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill "red", onSvgMouseDown (StartDragLinePoint pathId ControlPoint1), onSvgMouseUp (StopDragLinePoint pathId ControlPoint1) ] []
                , Svg.circle [ id (pathId ++ "-cp2"), cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill "red", onSvgMouseDown (StartDragLinePoint pathId ControlPoint2), onSvgMouseUp (StopDragLinePoint pathId ControlPoint2) ] []
                ]
            HoverLine -> 
               [ Svg.path [ id pathId, d dStr, stroke "black", strokeWidth "1", fill "none", onSvgClick (SelectElement pathId), Svg.Events.onMouseOut (UnhoverElement pathId) ] []
                , Svg.circle [ id (pathId ++ "-sp"), cx x1Str, cy y1Str, r "3", stroke "blue", fill "blue", onSvgMouseDown (StartDragLinePoint pathId StartPoint), onSvgMouseUp (StopDragLinePoint pathId StartPoint) ] []
                , Svg.circle [ id (pathId ++ "-ep"), cx x2Str, cy y2Str, r "3", stroke "blue", fill "blue", onSvgMouseDown (StartDragLinePoint pathId EndPoint), onSvgMouseUp (StopDragLinePoint pathId EndPoint) ] []
                , Svg.circle [ id (pathId ++ "-cp1"), cx xCp1Str, cy yCp1Str, r "3", stroke "red", fill "red", onSvgMouseDown (StartDragLinePoint pathId ControlPoint1), onSvgMouseUp (StopDragLinePoint pathId ControlPoint1) ] []
                , Svg.circle [ id (pathId ++ "-cp2"), cx xCp2Str, cy yCp2Str, r "3", stroke "red", fill "red", onSvgMouseDown (StartDragLinePoint pathId ControlPoint2), onSvgMouseUp (StopDragLinePoint pathId ControlPoint2) ] []
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
        EditLine id point pos -> 
            List.map (\line -> if line.id == id then createDragBezier line pos point else line) lines 
        DeleteLine id -> 
            List.filter (\line -> line.id /= id) lines 

getLinesFromHistory : List Action -> List BezierLine
getLinesFromHistory history = 
    List.foldr processAction [] history

createDragBezier : BezierLine -> Pos -> LinePoint -> BezierLine
createDragBezier bezier pos point = 
    case point of 
        StartPoint -> { bezier | start = pos }
        EndPoint -> { bezier | end = pos }
        ControlPoint1 -> { bezier | cp1 = pos }
        ControlPoint2 -> { bezier | cp2 = pos }

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
                Dragging { id, point } -> 
                    let
                        lines = getLinesFromHistory history
                        svgLines = lines |> List.filter (\line -> line.id /= id) |> toSvgLines
                    in        
                        case tryFind (\line -> line.id == id) lines of 
                            Nothing -> svgLines 
                            Just bezier -> 
                                let 
                                    dragBezier = createDragBezier bezier pos point
                                    dragLine = createSvgLine SelectedLine dragBezier
                                in 
                                    dragLine ++ svgLines
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
                Stopped { id } -> 
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
                Dragging _-> 
                    Html.text ("Dragging..." ++ model.status)
                Default -> 
                    Html.text ("Default..." ++ model.status)
                Selected _ -> 
                    Html.text ("Selected..." ++ model.status)
                Stopped _ -> 
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
                                    (triangleElements ++ svgElements) ] ] ] ] 
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
        Sub.batch 
        [ onMouseMove (Decode.map2 (\a -> \b -> MouseMove { x = a, y = b }) offsetX offsetY) ]
--        , onMouseUp (Decode.map2 (\a -> \b -> MouseUp { x = a, y = b }) offsetX offsetY) ]
     
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
