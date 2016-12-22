module Models exposing (..)

import List.Extra exposing (..)


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


initialModel_ : Model
initialModel_ =
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


nbInBoundary : Node -> Boundary -> Int
nbInBoundary n b =
    case b of
        [] ->
            0

        [ _ ] ->
            0

        _ :: _ :: _ ->
            List.sum
                (List.map
                    (\x ->
                        if x == n then
                            1
                        else
                            0
                    )
                    b
                )


order : Node -> Graph -> Int
order node graph =
    List.sum (List.concat (List.map (List.map (nbInBoundary node)) graph))


initialModel : Model
initialModel =
    { graph =
        [ [ [ 0 ], [ 1 ] ] ]
    , selectedRegion = Nothing
    , selectedBoundary = Nothing
    , selectedNode = Nothing
    }


selectable_ : Node -> Model -> Bool
selectable_ node model =
    case model.selectedNode of
        Nothing ->
            (order node model.graph) < 3

        Just n ->
            (order node model.graph) < 2 || (n /= node && (order node model.graph) < 3)


selectable : Node -> Region -> Model -> Bool
selectable node region model =
    case model.selectedRegion of
        Nothing ->
            selectable_ node model

        Just r ->
            (selectable_ node model) && (region == r)


selected_ : Node -> Model -> Bool
selected_ node model =
    case model.selectedNode of
        Nothing ->
            False

        Just n ->
            node == n


selected : Node -> Region -> Model -> Bool
selected node region model =
    case model.selectedRegion of
        Nothing ->
            selected_ node model

        Just r ->
            (selected_ node model) && (region == r)


cartesianSquare : List a -> List ( a, a )
cartesianSquare l =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) l)
        l


playable : Graph -> Bool
playable graph =
    let
        connectable ( n1, n2 ) =
            (order n1 graph < 2) || (n1 /= n2 && (order n1 graph) < 3 && (order n2 graph) < 3)

        alive region =
            List.foldl (||) False (List.map connectable (cartesianSquare <| List.Extra.unique <| List.concat <| region))
    in
        List.foldl (||) False (List.map alive graph)
