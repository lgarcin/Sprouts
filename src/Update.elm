module Update exposing (cycle, merge, newNode, oneBoundaryMove, split, twoBoundaryMove, update)

import List.Extra exposing (..)
import Messages exposing (..)
import Models exposing (..)


twoBoundaryMove : Node -> Node -> Boundary -> Boundary -> Region -> Graph -> Graph
twoBoundaryMove n1 n2 b1 b2 r g =
    setIf (\rr -> rr == r) (merge n1 n2 (newNode g) b1 b2 :: remove b2 (remove b1 r)) g


merge : Node -> Node -> Node -> Boundary -> Boundary -> Boundary
merge n1 n2 x b1 b2 =
    let
        ( bb1, bb2 ) =
            ( cycle n1 b1, cycle n2 b2 )
    in
    case ( bb1, bb2 ) of
        ( [], [] ) ->
            []

        ( _, [] ) ->
            bb1

        ( [], _ ) ->
            bb2

        ( [ _ ], [ _ ] ) ->
            n1 :: x :: n2 :: [ x ]

        ( _ :: _ :: _, [ _ ] ) ->
            bb1 ++ (n1 :: x :: n2 :: [ x ])

        ( [ _ ], _ :: _ :: _ ) ->
            (n1 :: x :: bb2) ++ [ n2, x ]

        ( _ :: _ :: _, _ :: _ :: _ ) ->
            bb1 ++ (n1 :: x :: bb2) ++ [ n2, x ]


cycle : Node -> Boundary -> Boundary
cycle x l =
    case l of
        [] ->
            []

        xs :: li ->
            if xs == x then
                l

            else
                cycle x (li ++ [ xs ])


oneBoundaryMove : Node -> Node -> Boundary -> Region -> (Boundary -> Bool) -> Graph -> Graph
oneBoundaryMove n1 n2 b r func g =
    let
        ( b1, b2 ) =
            split n1 n2 (newNode g) b
    in
    (b1 :: (List.filter func r |> List.Extra.remove b)) :: (b2 :: (List.filter (not << func) r |> List.Extra.remove b)) :: (g |> List.Extra.remove r)


split : Node -> Node -> Node -> Boundary -> ( Boundary, Boundary )
split n1 n2 x b =
    case b of
        [] ->
            ( [], [] )

        [ _ ] ->
            ( [ n1, x ], [ n2, x ] )

        _ :: _ ->
            ( List.Extra.dropWhile (\v -> v /= n2) (cycle n1 b) ++ [ n1, x ], List.Extra.takeWhile (\v -> v /= n2) (cycle n1 b) ++ [ n2, x ] )


newNode : Graph -> Node
newNode g =
    case List.maximum <| List.concat <| List.concat <| g of
        Nothing ->
            0

        Just v ->
            v + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select n b r ->
            case model.selectedNode of
                Nothing ->
                    ( { model | selectedNode = Just n, selectedBoundary = Just b, selectedRegion = Just r }, Cmd.none )

                Just nn ->
                    case ( model.selectedBoundary, model.selectedRegion ) of
                        ( Nothing, _ ) ->
                            ( model, Cmd.none )

                        ( _, Nothing ) ->
                            ( model, Cmd.none )

                        ( Just bb, Just rr ) ->
                            if rr /= r then
                                ( model, Cmd.none )

                            else if bb == b then
                                ( { model | graph = oneBoundaryMove nn n bb rr (\x -> True) model.graph, selectedNode = Nothing, selectedBoundary = Nothing, selectedRegion = Nothing }, Cmd.none )

                            else
                                ( { model | graph = twoBoundaryMove nn n bb b rr model.graph, selectedNode = Nothing, selectedBoundary = Nothing, selectedRegion = Nothing }, Cmd.none )

        KeyPressed "Shift" ->
            ( { model | selectedNode = Nothing, selectedBoundary = Nothing, selectedRegion = Nothing }, Cmd.none )

        KeyPressed _ ->
            ( model, Cmd.none )
