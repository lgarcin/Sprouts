module Models exposing (..)


type alias Node =
    Int


type alias Boundary =
    List Node


type alias Region =
    List Boundary


type alias Graph =
    List Region


type alias Model =
    { graph : Graph
    , selectedRegion : Maybe Region
    , selectedBoundary : Maybe Boundary
    , selectedNode : Maybe Node
    }


initialModel' : Model
initialModel' =
    { graph =
        [ [ [ 1, 12 ] ]
        , [ [ 1, 12 ]
          , [ 2, 14, 13, 3, 13, 14 ]
          ]
        , [ [ 4 ]
          , [ 3, 15, 6, 16, 7, 17, 6, 15, 3, 13 ]
          ]
        , [ [ 5 ]
          , [ 8, 18, 9, 19, 10, 19, 9, 21, 11, 20, 11, 21, 9, 18 ]
          , [ 6, 17, 7, 16 ]
          ]
        , [ [ 11, 20 ] ]
        ]
    , selectedRegion = Nothing
    , selectedBoundary = Nothing
    , selectedNode = Nothing
    }


initialModel : Model
initialModel =
    { graph =
        [ [ [ 0 ], [ 1 ] ] ]
    , selectedRegion = Nothing
    , selectedBoundary = Nothing
    , selectedNode = Nothing
    }
