module ViewBis exposing (..)

import Html exposing (..)
import Messages exposing (..)
import Models exposing (..)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown, onMouseOut, onMouseOver)



-- Il faut gérer tout en même temps : pas de possibilité de tracer indépendamment les frontières
