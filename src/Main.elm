module Main exposing (..)

import Browser
import DayTime exposing (DayTime)
import Debug exposing (todo)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Task exposing (Task, perform)
import Tuple



-- Model


type alias Model =
    { commandLog : List ( WorkCommand, DayTime )
    , rawTimeInput : String
    , error : Maybe String
    , workStatus : WorkStatus
    , executingCommand : Maybe WorkCommand
    }


type WorkStatus
    = BeforeWork
    | Working
    | Resting
    | AfterWork



-- Msg


type Msg
    = ClearErrorMessage
    | WorkCommand WorkCommand
    | RecordTime (Result String ( WorkStatus, DayTime ))


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
            { commandLog = []
            , rawTimeInput = ""
            , error = Nothing
            , workStatus = BeforeWork
            , executingCommand = Nothing
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
        ClearErrorMessage ->
            ( { model | error = Nothing }, Cmd.none )

        RecordTime res ->
            case ( model.executingCommand, res ) of
                ( Just cmd, Ok ( ws, dt ) ) ->
                    ( { model | workStatus = ws, commandLog = List.append model.commandLog [ ( cmd, dt ) ], executingCommand = Nothing }, Cmd.none )

                ( Just _, Err mes ) ->
                    ( { model | error = Just mes, executingCommand = Nothing }, Cmd.none )

                _ ->
                    ( { model | executingCommand = Nothing, error = Just "unknown error" }, Cmd.none )

        WorkCommand command ->
            let
                n : Task x (Result String DayTime)
                n =
                    Task.map (Result.fromMaybe "failed to get current time") DayTime.now

                r : Task x (Result String ( WorkStatus, DayTime ))
                r =
                    Task.map (Result.map2 Tuple.pair <| updateWorkStatus model.workStatus command) n
            in
            ( { model | executingCommand = Just command }, perform RecordTime r )


updateWorkStatus : WorkStatus -> WorkCommand -> Result String WorkStatus
updateWorkStatus status command =
    case ( status, command ) of
        ( BeforeWork, StartWork ) ->
            Ok Working

        ( Working, StartRest ) ->
            Ok Resting

        ( Resting, ResumeWork ) ->
            Ok Working

        ( Working, FinishWork ) ->
            Ok AfterWork

        _ ->
            Err "invalid workstate transition"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewCommandButton model.workStatus
        , div [] [ text <| Maybe.withDefault "-" model.error ]
        , div [] [ viewWorkingTime model ]
        , div [] <| List.map viewWorkCommandLog model.commandLog
        ]


calculateWorkingTime : List ( WorkCommand, DayTime ) -> Result String DayTime.Period
calculateWorkingTime commandLogs =
    let
        initPeriod =
            { hour = 0, minute = 0 }

        go : ( WorkStatus, DayTime, DayTime.Period ) -> List ( WorkCommand, DayTime ) -> Result String DayTime.Period
        go ( status, prevTime, sum ) logs =
            case logs of
                [] ->
                    if List.member status [ Resting, AfterWork ] then
                        Ok sum

                    else
                        Err "working now"

                ( command, time ) :: remains ->
                    case updateWorkStatus status command of
                        Ok next ->
                            if List.member command [ StartRest, FinishWork ] then
                                go ( next, time, DayTime.addPeriod sum <| DayTime.diff prevTime time ) remains

                            else
                                go ( next, time, sum ) remains

                        Err _ ->
                            Err "invalid workstatus transition"
    in
    case commandLogs of
        [] ->
            Ok initPeriod

        ( StartWork, time ) :: remains ->
            go ( Working, time, initPeriod ) remains

        _ ->
            Err "invalid command logs"


commandName : WorkCommand -> String
commandName command =
    case command of
        StartWork ->
            "仕事開始"

        StartRest ->
            "休憩開始"

        ResumeWork ->
            "仕事再開"

        FinishWork ->
            "仕事終了"


viewWorkCommandLog : ( WorkCommand, DayTime ) -> Html msg
viewWorkCommandLog ( command, time ) =
    div [] [ text (DayTime.show time ++ " -> " ++ commandName command) ]


viewWorkingTime : Model -> Html msg
viewWorkingTime model =
    case calculateWorkingTime model.commandLog of
        Ok period ->
            div []
                [ span [] [ text "作業時間: " ]
                , span [] [ text (DayTime.showPeriod period) ]
                ]

        Err message ->
            div []
                [ span [] [ text "エラー" ]
                , span [] [ text message ]
                ]


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
