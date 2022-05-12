module Main exposing (..)

import Browser
import DayTime exposing (DayTime)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Task exposing (perform)



-- Model


type alias Model =
    { log : List DayTime
    , rawTimeInput : String
    , error : Maybe String
    , workStatus : WorkStatus
    }


type WorkStatus
    = BeforeWork
    | Working
    | Resting
    | AfterWork



-- Msg


type Msg
    = AddLogReq
    | AddLog (Maybe DayTime)
    | AddRawTime
    | ChangeRawTimeInput String
    | ClearErrorMessage
    | WorkCommand WorkCommand


type WorkCommand
    = StartWork
    | StartRest
    | ResumeWork
    | FinishWork



-- INIT


init : a -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { log = []
            , rawTimeInput = ""
            , error = Nothing
            , workStatus = BeforeWork
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
            let
                newLogs =
                    case time of
                        Nothing ->
                            model.log

                        Just log ->
                            log :: model.log
            in
            ( { model | log = newLogs }, clearError )

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

        WorkCommand command ->
            updateWorkStatus command model


updateWorkStatus : WorkCommand -> Model -> ( Model, Cmd Msg )
updateWorkStatus command model =
    case command of
        StartWork ->
            ( { model | workStatus = Working }, Cmd.none )

        StartRest ->
            ( { model | workStatus = Resting }, Cmd.none )

        ResumeWork ->
            ( { model | workStatus = Working }, Cmd.none )

        FinishWork ->
            ( { model | workStatus = AfterWork }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddLogReq ] [ text "save now" ]
        , viewCommandButton model.workStatus
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


viewCommandButton : WorkStatus -> Html Msg
viewCommandButton status =
    case status of
        BeforeWork ->
            div []
                [ button [ onClick (WorkCommand StartWork) ] [ text "仕事開始" ]
                ]

        Working ->
            div []
                [ button [ onClick (WorkCommand StartRest) ] [ text "休憩" ]
                , button [ onClick (WorkCommand FinishWork) ] [ text "仕事終了" ]
                ]

        Resting ->
            div []
                [ button [ onClick (WorkCommand ResumeWork) ] [ text "仕事再開" ]
                ]

        AfterWork ->
            div []
                [ button [ disabled True ] [ text "終了済み" ]
                ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
