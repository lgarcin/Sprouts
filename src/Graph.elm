module Graph exposing (..)

import Html exposing (text)
import List.Extra exposing (..)


type alias Node =
    Int


type alias Boundary =
    List Node


type alias Region =
    List Boundary


type alias Graph =
    List Region


twoBoundaryMove : Node -> Node -> Graph -> Graph
twoBoundaryMove n1 n2 g =
    let
        r =
            Maybe.withDefault [] (List.Extra.find (\r -> List.member n1 (List.concat r) && List.member n2 (List.concat r)) g)

        b1 =
            Maybe.withDefault [] (List.Extra.find (List.member n1) r)

        b2 =
            Maybe.withDefault [] (List.Extra.find (List.member n2) r)
    in
        replaceIf (\rr -> rr == r) ((merge n1 n2 (newNode g) b1 b2) :: (remove b2 (remove b1 r))) g


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


oneBoundaryMove : Node -> Node -> Region -> (Boundary -> Bool) -> Graph -> Graph
oneBoundaryMove n1 n2 r func g =
    let
        b =
            Maybe.withDefault [] (List.Extra.find (\b -> List.member n1 b && List.member n2 b) r)

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
            ( List.Extra.takeWhileRight (\x -> x /= n2) (cycle n1 b) ++ [ n1, x, n2 ], (List.Extra.takeWhile (\x -> x /= n2) (cycle n1 b)) ++ [ n2, x ] )


newNode : Graph -> Node
newNode g =
    case List.maximum <| List.concat <| List.concat <| g of
        Nothing ->
            0

        Just v ->
            v + 1


main : Html.Html a
main =
    text (toString (split 0 0 1 [ 0 ]))
