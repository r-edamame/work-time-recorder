module Main exposing (..)

import Browser
import DayTime exposing (DayTime)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Task exposing (perform)



-- Model


type alias Model =
    { log : List DayTime
    , rawTimeInput : String
    , error : Maybe String
    }



-- Msg


type Msg
    = AddLogReq
    | AddLog DayTime
    | AddRawTime
    | ChangeRawTimeInput String
    | ClearErrorMessage



-- INIT


init : a -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { log = []
            , rawTimeInput = ""
            , error = Nothing
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearError =
            perform identity <| Task.succeed ClearErrorMessage
    in
    case msg of
        AddLogReq ->
            ( model, perform AddLog DayTime.now )

        AddLog time ->
            ( { model | log = time :: model.log }, clearError )

        AddRawTime ->
            case DayTime.parseDayTime model.rawTimeInput of
                Ok time ->
                    ( { model | log = time :: model.log, rawTimeInput = "" }, clearError )

                Err message ->
                    ( { model | error = Just message }, Cmd.none )

        ChangeRawTimeInput input ->
            ( { model | rawTimeInput = input }, Cmd.none )

        ClearErrorMessage ->
            ( { model | error = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddLogReq ] [ text "save now" ]
        , div []
            [ input [ onInput ChangeRawTimeInput, value model.rawTimeInput ] []
            , button [ onClick AddRawTime ] [ text "save" ]
            ]
        , div [] [ text <| Maybe.withDefault "-" model.error ]
        , div [] <| List.map viewTimeLog model.log
        ]


viewTimeLog : DayTime -> Html Msg
viewTimeLog time =
    div [] [ text (DayTime.show time) ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
