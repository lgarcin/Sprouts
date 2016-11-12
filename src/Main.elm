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
    { id : Int
    , x : Int
    , y : Int
    }

type alias Model =
    { list : List Point
    , selected : Maybe Int
    , hovered : Maybe Int
    }

model : Model
model =
    { list = [{ id = 0
              , x = 50
              , y = 50
              }
              ,{ id = 1
              , x = 70
              , y = 30
              }]
    , selected = Nothing
    , hovered = Nothing
    }

-- Update

type Msg = Hovered Int | Selected  Int | Unhovered
update : Msg -> Model -> Model
update msg model =
    case msg of
        Hovered id -> { model | hovered = Just id }
        Unhovered -> { model | hovered = Nothing}
        Selected id -> { model | selected = Just id }


-- View

view : Model -> Html Msg
view model =
    let
        drawPoint : Point -> Svg Msg 
        drawPoint point =
            circle [ cx (toString point.x)
                , cy (toString point.y)
                , r "2"
                , fill <| if Just point.id == model.selected then "green" else if Just point.id == model.hovered then "blue" else "red"
                , onMouseOver <| Hovered point.id
                , onMouseOut <| Unhovered
                , onMouseDown <| Selected point.id ] []
    in
        Html.div []
        [
            svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
            (List.map drawPoint model.list)
        ]
