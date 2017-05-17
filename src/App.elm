module App exposing (..)

import Html exposing (program, div, Html)


type alias Model =
    {}


type Msg
    = NoOp


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    {} ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] []
