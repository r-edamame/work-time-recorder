module Main exposing (..)

import Browser
import Browser.Dom exposing (Error)
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
    | RecordTime ( WorkCommand, WorkStatus, DayTime )
    | WorkCommandWithRawInput WorkCommand
    | ErrorOccured String
    | ChangeRawTimeInput String


type WorkCommand
    = StartWork
    | StartRest
    | ResumeWork
    | FinishWork


performWithCatch : (a -> Msg) -> Task String a -> Cmd Msg
performWithCatch msg task =
    Task.map msg task
        |> Task.onError (Task.succeed << ErrorOccured)
        |> Task.perform identity



-- INIT


init : a -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { commandLog = []
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
        clearerror =
            perform identity <| Task.succeed ClearErrorMessage
    in
    case msg of
        ErrorOccured err ->
            ( { model | error = Just err }, Cmd.none )

        ClearErrorMessage ->
            ( { model | error = Nothing }, Cmd.none )

        RecordTime ( cmd, ws, dt ) ->
            ( { model | workStatus = ws, commandLog = List.append model.commandLog [ ( cmd, dt ) ] }, Cmd.none )

        WorkCommand command ->
            case updateWorkStatus model.workStatus command of
                Ok status ->
                    let
                        m : Task String ( WorkCommand, WorkStatus, DayTime )
                        m =
                            Task.map (\dt -> ( command, status, dt )) DayTime.now
                    in
                    ( model, performWithCatch RecordTime m )

                Err err ->
                    ( { model | error = Just "invalid workstate transition" }, Cmd.none )

        WorkCommandWithRawInput command ->
            let
                rdt =
                    DayTime.parseDayTime model.rawTimeInput

                rst =
                    updateWorkStatus model.workStatus command
            in
            case Result.map2 Tuple.pair rdt rst of
                Ok ( dt, st ) ->
                    ( { model | workStatus = st, commandLog = List.append model.commandLog [ ( command, dt ) ], rawTimeInput = "" }, Cmd.none )

                Err err ->
                    ( { model | error = Just err }, Cmd.none )

        ChangeRawTimeInput input ->
            ( { model | rawTimeInput = input }, Cmd.none )


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
        [ div [] <| viewCommandButton WorkCommand model.workStatus
        , div [] [ viewDirectInputWorkCommand model ]
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
                    -- 休憩か作業終了で終わってる場合は結果を返す
                    if List.member status [ Resting, AfterWork ] then
                        Ok sum

                    else
                        Err "working now"

                ( command, time ) :: remains ->
                    case updateWorkStatus status command of
                        Ok next ->
                            -- 休憩に入るか作業が終わるときに作業時間を加算
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


viewDirectInputWorkCommand : Model -> Html Msg
viewDirectInputWorkCommand model =
    div []
        [ input [ onInput ChangeRawTimeInput, value model.rawTimeInput ] []
        , span [] <| viewCommandButton WorkCommandWithRawInput model.workStatus
        ]


availableCommand : WorkStatus -> List WorkCommand
availableCommand status =
    case status of
        BeforeWork ->
            [ StartWork ]

        Working ->
            [ StartRest, FinishWork ]

        Resting ->
            [ ResumeWork ]

        AfterWork ->
            []


viewCommandButton : (WorkCommand -> Msg) -> WorkStatus -> List (Html Msg)
viewCommandButton msg status =
    let
        buttonInner com =
            [ text <| commandName com ]

        comButton com =
            button [ onClick (msg com) ] <| buttonInner com
    in
    case status of
        AfterWork ->
            [ button [ disabled True ] [ text "終了済み" ] ]

        _ ->
            List.map comButton <| availableCommand status



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
