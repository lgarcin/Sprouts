module Main exposing (..)

import Models exposing (..)
import Messages exposing (..)
import View exposing (..)
import Update exposing (..)
import Html.App exposing (..)
import Keyboard


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
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
