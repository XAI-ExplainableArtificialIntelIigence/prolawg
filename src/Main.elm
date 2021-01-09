module Main exposing (..)

import Browser exposing (sandbox)
import Element exposing (layout, text)
import Html exposing (Html)

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    String


type Msg
    = NoMsg


init : Model
init =
    ""


view : Model -> Html Msg
view model =
    layout [] (text model)


update : Msg -> Model -> Model
update msg model =
    model
