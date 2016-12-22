module ViewBis exposing (..)

import Models exposing (..)
import Messages exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOver, onMouseOut, onMouseDown)
import Svg exposing (Svg, svg, circle, rect)
import Html exposing (..)

-- Il faut gérer tout en même temps : pas de possibilité de tracer indépendamment les frontières 