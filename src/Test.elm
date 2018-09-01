module Main exposing (Model, Msg(..), Point, init, initialModel, main, subscriptions, update, view)

import Html exposing (Html)
import Html.App as App
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown, onMouseOut, onMouseOver)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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


initialModel : Model
initialModel =
    { list =
        [ { id = 0
          , x = 50
          , y = 50
          }
        , { id = 1
          , x = 70
          , y = 30
          }
        ]
    , selected = Nothing
    , hovered = Nothing
    }



-- Update


type Msg
    = Hovered Int
    | Selected Int
    | Unhovered


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hovered id ->
            ( { model | hovered = Just id }, Cmd.none )

        Unhovered ->
            ( { model | hovered = Nothing }, Cmd.none )

        Selected id ->
            ( { model | selected = Just id }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        drawPoint : Point -> Svg Msg
        drawPoint point =
            circle
                [ cx (toString point.x)
                , cy (toString point.y)
                , r "2"
                , fill <|
                    if Just point.id == model.selected then
                        "green"

                    else if Just point.id == model.hovered then
                        "blue"

                    else
                        "red"
                , onMouseOver <| Hovered point.id
                , onMouseOut <| Unhovered
                , onMouseDown <| Selected point.id
                ]
                []
    in
    Html.div []
        [ svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
            (List.map drawPoint model.list)
        ]
