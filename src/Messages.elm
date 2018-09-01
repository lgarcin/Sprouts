module Messages exposing (Msg(..))

import Models exposing (..)


type Msg
    = Select Node Boundary Region
    | KeyPressed String
