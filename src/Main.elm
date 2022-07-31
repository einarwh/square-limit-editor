module Main exposing (main)

import Browser
import Browser.Events exposing (onClick)
import Html exposing (Html, text, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode

type alias Pos = { x : Float, y : Float }

type alias BezierLine = { start : Pos, cp1 : Pos, cp2 : Pos, end : Pos }

type alias Model =
    { drawing: Bool, pos: Maybe Pos, lines: List BezierLine, message : String }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { message = "Click on the page", drawing = False, lines = [], pos = Nothing }, Cmd.none )


type alias Msg = { x : Float, y : Float }

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        { x, y } ->
            case model.pos of 
                Just pos -> 
                  -- Currently drawing, end line now and add to lines.
                    let line = createBezierLine pos {x=x, y=y} 
                    in 
                        ( { message =
                                "Complete a line at coordinates "
                                ++ String.fromFloat x
                                ++ ", "
                                ++ String.fromFloat y
                          , drawing = False
                          , lines = line :: model.lines
                          , pos = Nothing
                          }
                        , Cmd.none
                        )
                Nothing ->  
                -- Not currently drawing, start drawing now.
                  ( { message =
                        "Start drawing a line at coordinates "
                        ++ String.fromFloat x
                        ++ ", "
                        ++ String.fromFloat y
                    , drawing = True
                    , lines = model.lines
                    , pos = Just {x=x, y=y}
                    }
                  , Cmd.none
                  )


view : Model -> Html Msg
view model =
    let
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
        drawText = if model.drawing then "drawing" else "not drawing"
        lineCount = List.length model.lines |> String.fromInt
        lines = List.concatMap createSvgLine model.lines
    in
        div []
          [
            Html.text (model.message ++ " " ++ drawText ++ " lines: " ++ lineCount)
          , svg
                [ width "500"
                , height "500"
                , viewBox "0 0 500 500"
                ]
                lines
          ]


subscriptions : Model -> Sub Msg
subscriptions model =
    onClick
        (Decode.map2 Msg
            (Decode.field "pageX" Decode.float)
            (Decode.field "pageY" Decode.float)
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
