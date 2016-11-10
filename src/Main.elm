module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Svg.Attributes exposing (..)
import Svg exposing (..)


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

type alias Model = Point

model : Model
model =
    { x = 50
    , y = 50
    }

-- Update

type Msg = Stop
update : Msg -> Model -> Model
update msg model = model

-- View

view : Model -> Html Msg
view model =
    Html.div []
    [
    svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
      [ circle [ cx (toString model.x), cy (toString model.y), r "5", fill "#0B79CE" ] []
      ]
    ]