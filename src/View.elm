module View exposing (..)

import Models exposing (..)
import Messages exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOver, onMouseOut, onMouseDown)
import Svg exposing (Svg, svg, circle, rect)
import Html exposing (..)


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

                drawBoundary i boundary =
                    let
                        pointStyle node =
                            (if selectable node then
                                [ fill "orange", onMouseDown <| Select node boundary region ]
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
                                [ circle ([ cx (toString (i * 30)), cy "0", r "5" ] ++ (pointStyle node)) []
                                , Svg.text' [ x (toString (i * 30)), y "0" ] [ Svg.text (toString node) ]
                                ]
                    in
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
                    ((rect [ x "0", y "-15", rx "10", ry "10", width "500", height (toString (10 + (List.length region) * 40)), fill "blue", opacity getRegionOpacity ] [])
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
