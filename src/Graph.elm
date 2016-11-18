module Graph exposing (..)

import Html exposing (..)
import Html.App exposing (..)
import List.Extra exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOver, onMouseOut, onMouseDown)
import Svg exposing (Svg, svg, circle, rect)


type alias Node =
    Int


type alias Boundary =
    List Node


type alias Region =
    List Boundary


type alias Graph =
    List Region


type alias Model =
    Graph


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
order n g =
    List.sum (List.concat (List.map (List.map (nbInBoundary n)) g))


connectable : Node -> Node -> Graph -> Bool
connectable n1 n2 g =
    (n1 == n2 && (order n1 g) < 2) || (n1 /= n2 && (order n1 g) < 3 && (order n2 g) < 3)


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
            ( List.Extra.dropWhile (\x -> x /= n2) (cycle n1 b) ++ [ n1, x ], (List.Extra.takeWhile (\x -> x /= n2) (cycle n1 b)) ++ [ n2, x ] )


newNode : Graph -> Node
newNode g =
    case List.maximum <| List.concat <| List.concat <| g of
        Nothing ->
            0

        Just v ->
            v + 1


type Msg
    = Move Node Node


drawNode : Int -> Node -> Svg Msg
drawNode ind n =
    Svg.g [] [ circle [ cx (toString (ind * 30)), cy "0", r "5", fill "red" ] [], Svg.text' [ x (toString (ind * 30)), y "0" ] [ Svg.text (toString n) ] ]


drawBoundary : Int -> Boundary -> Svg Msg
drawBoundary ind b =
    Svg.g [ transform <| "translate" ++ (toString ( 10, ind * 30 )) ] (List.indexedMap drawNode b)


drawRegion : Region -> List (Svg Msg)
drawRegion r =
    (rect [ x "0", y "-15", rx "10", ry "10", width "500", height (toString ((List.length r) * 30)), fill "blue", opacity "0.1" ] []) :: (List.indexedMap drawBoundary r)


drawGraph : Graph -> List (Svg Msg)
drawGraph g =
    let
        dr i r =
            Svg.g [ transform <| "translate" ++ (toString ( 10, 30 + 40 * List.sum (List.map List.length (List.take i g)) )) ] (drawRegion r)
    in
        List.indexedMap dr g


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg [ viewBox "0 0 600 900", Svg.Attributes.width "600px" ]
            (drawGraph model)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move n1 n2 ->
            ( twoBoundaryMove n1 n2 model, Cmd.none )


initialModel : Model
initialModel =
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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
