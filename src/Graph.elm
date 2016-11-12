module Graph exposing (..)

import Set exposing (Set)

type alias Node = Int

type alias Edge = (Node, Node)

type alias Face = List Node

type alias Graph =
    { nodes : Set Node
    , edges : Set Edge
    , faces : Set Face
    }

insert : Node -> Node -> Node -> Face -> Graph -> Graph
insert n n1 n2 f g =
    { g | nodes = Set.insert n g.nodes
        , edges = g.edges |> Set.insert (n,n1) |> Set.insert (n,n2)
        , faces = g.faces |> Set.remove f |> Set.union (Set.fromList (split n n1 n2 f))
    }

split : Node -> Node -> Node -> Face -> List Face
split n n1 n2 f =
    if List.head f == Just n1 then
        case split' n2 [] f of
            (f1,f2) -> [n2::n::f1,n1::n::f2]
    else
        split n n1 n2 (cycle f)

cycle : List a -> List a
cycle l =
    case l of
        [] -> []
        xs::li -> li ++ [xs]

split' : Node -> Face -> Face -> (Face,Face)
split' n f1 f2 =
    if List.head f2 == Just n then
        (f1,f2)
    else
        case f2 of
            [] -> (f1,f2)
            xs::li -> split' n (f1 ++ [xs]) (li)
