module Main exposing (..)

import Browser
import DayTime exposing (DayTime)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task exposing (perform)



-- Model


type alias Model =
    { log : List DayTime
    }



-- Msg


type Msg
    = AddLogReq
    | AddLog DayTime



-- INIT


init : a -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { log = []
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLogReq ->
            ( model, perform AddLog DayTime.now )

        AddLog time ->
            ( { model | log = time :: model.log }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddLogReq ] [ text "save" ]
        , div [] <| List.map viewTimeLog model.log
        ]


viewTimeLog : DayTime -> Html Msg
viewTimeLog time =
    div [] [ text (DayTime.show time) ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
