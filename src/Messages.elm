module Messages exposing (..)

import Models exposing (..)
import Keyboard exposing (..)


type Msg
    = Select Node Boundary Region
    | KeyMsg KeyCode
