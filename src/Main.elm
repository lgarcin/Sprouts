module Main exposing (init, main, subscriptions)

import Browser
import Browser.Events as Events
import Json.Decode as Decode
import Messages exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyPress (Decode.map KeyPressed keyDecoder)
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Régler le problème de la touche Escape
