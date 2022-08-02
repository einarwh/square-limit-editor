module Help exposing (help)

import Box exposing (..)
import Picture exposing (..)
import Gather exposing (gatherBoxes)
import Recursion exposing (sideBoxes, cornerBoxes)
import Letter exposing (..)
import Figure exposing (..)
import Fishy exposing (fishShapes)
import Triangular exposing (..)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Decor exposing (render)

help : Html msg
help = 
  let 
    box = { a = { dx = 100.0, dy = 100.0 }
          , b = { dx = 200.0, dy = 0.0 }
          , c = { dx = 0.0, dy = 200.0 } }
    f = createPicture fLetter
  in     
    box |> ttile f
        |> render []