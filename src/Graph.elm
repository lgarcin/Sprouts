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
    { graph : Graph
    , selectedRegion : Maybe Region
    , selectedNode : Maybe Node
    }


twoBoundaryMove : Node -> Node -> Region -> Graph -> Graph
twoBoundaryMove n1 n2 r g =
    let
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
    = SelectRegion Region
    | SelectNode Node


drawGraph : Model -> List (Svg Msg)
drawGraph model =
    let
        order node =
            List.sum (List.concat (List.map (List.map (nbInBoundary node)) model.graph))

        drawRegion ind region =
            let
                selectable' node =
                    case model.selectedNode of
                        Nothing ->
                            (order node) < 3

                        Just n ->
                            (order node) < 2 || (n /= node && (order node) < 3)

                selectable node =
                    case model.selectedRegion of
                        Nothing ->
                            selectable' node

                        Just r ->
                            (selectable' node) && (region == r)

                selected' node =
                    case model.selectedNode of
                        Nothing ->
                            False

                        Just n ->
                            node == n

                selected node =
                    case model.selectedRegion of
                        Nothing ->
                            selected' node

                        Just r ->
                            (selected' node) && (region == r)

                pointStyle node =
                    (if selectable node then
                        [ fill "orange" ]
                     else
                        [ fill "green" ]
                    )
                        ++ (if selected node then
                                [ stroke "black" ]
                            else
                                []
                           )

                drawNode i node =
                    Svg.g []
                        [ circle ([ cx (toString (i * 30)), cy "0", r "5", onMouseDown <| SelectNode node ] ++ (pointStyle node)) []
                        , Svg.text' [ x (toString (i * 30)), y "0" ] [ Svg.text (toString node) ]
                        ]

                drawBoundary i boundary =
                    Svg.g [ transform <| "translate" ++ (toString ( 20, 10 + i * 40 )) ]
                        ((rect [ x "-10", y "-15", rx "3", ry "3", width (toString ((List.length boundary) * 30)), height "30", fill "yellow" ] [])
                            :: (List.indexedMap drawNode boundary)
                        )

                getRegionOpacity =
                    if Just region == model.selectedRegion then
                        "0.7"
                    else
                        "0.1"
            in
                Svg.g [ transform <| "translate" ++ (toString ( 10, 30 + 30 * ind + 40 * List.sum (List.map List.length (List.take ind model.graph)) )) ]
                    ((rect [ x "0", y "-15", rx "10", ry "10", width "500", height (toString (10 + (List.length region) * 40)), fill "blue", opacity getRegionOpacity, onMouseOver <| SelectRegion region ] [])
                        :: (List.indexedMap drawBoundary region)
                    )
    in
        List.indexedMap drawRegion model.graph


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg [ viewBox "0 0 900 900", Svg.Attributes.width "900px" ]
            (drawGraph model)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectRegion r ->
            case model.selectedNode of
                Nothing ->
                    ( { model | selectedRegion = Just r }, Cmd.none )

                Just _ ->
                    ( model, Cmd.none )

        SelectNode n ->
            case model.selectedNode of
                Nothing ->
                    ( { model | selectedNode = Just n }, Cmd.none )

                Just _ ->
                    case ( model.selectedNode, model.selectedRegion ) of
                        ( Just sn, Just sr ) ->
                            ( { model | graph = twoBoundaryMove sn n sr model.graph, selectedNode = Nothing }, Cmd.none )

                        ( _, _ ) ->
                            ( model, Cmd.none )


initialModel : Model
initialModel =
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
    , selectedNode = Nothing
    }


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
