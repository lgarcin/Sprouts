module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOver, onMouseOut, onMouseDown)
import Svg exposing (Svg, svg, circle)


main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model

type alias Point =
    { x : Int
    , y : Int
    }

type alias Model =
    { list : List Point
    , selected : Maybe Point
    , hovered : Maybe Point
    }

model : Model
model =
    { list = [{ x = 50
              , y = 50
              }
              ,{ x = 70
              , y = 30
              }]
    , selected = Nothing
    , hovered = Nothing
    }

-- Update

type Msg = Hovered Point | Selected  Point | Unhovered
update : Msg -> Model -> Model
update msg model =
    case msg of
        Hovered point -> { model | hovered = Just point }
        Unhovered -> { model | hovered = Nothing}
        Selected point -> { model | selected = Just point }


-- View

view : Model -> Html Msg
view model =
    let
        drawPoint : Point -> Svg Msg 
        drawPoint point =
            circle [ cx (toString point.x)
                , cy (toString point.y)
                , r "2"
                , fill <| if Just point == model.selected then "green" else if Just point == model.hovered then "blue" else "red"
                , onMouseOver <| Hovered point
                , onMouseOut <| Unhovered
                , onMouseDown <| Selected point ] []
    in
        Html.div []
        [
            svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
            (List.map drawPoint model.list)
        ]
