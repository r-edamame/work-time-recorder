module DayTime exposing (..)

import Regex exposing (Regex)
import Task exposing (Task)
import Time exposing (Posix)


type alias DayTime =
    { hour : Int
    , minute : Int
    , nextDay : Bool
    }


dayTimeRegex : Regex
dayTimeRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "^(\\d\\d):(\\d\\d)$"


parseDayTime : String -> Result String DayTime
parseDayTime src =
    let
        parse s =
            case Regex.find dayTimeRegex s of
                [ { submatches } ] ->
                    Ok submatches

                _ ->
                    Err "invalid time form"

        invalidError =
            Err "invalid form"
    in
    case parse src of
        Ok [ Just h, Just m ] ->
            let
                mhour =
                    String.toInt h

                mminute =
                    String.toInt m
            in
            case Maybe.map2 validateDayTime mhour mminute of
                Just (Ok dt) ->
                    Ok dt

                Just (Err message) ->
                    Err message

                Nothing ->
                    Err "number parse failed"

        Ok _ ->
            Err "unknown parse error"

        Err mes ->
            Err mes


validateDayTime : Int -> Int -> Result String DayTime
validateDayTime h m =
    let
        hourValid =
            0 <= h && h <= 24 + 7

        minuteValid =
            0 <= m && m <= 60
    in
    case ( hourValid, minuteValid ) of
        ( False, False ) ->
            Err "ValidationError: invalid hour and minute"

        ( False, _ ) ->
            Err "ValidationError: invalid hour"

        ( True, False ) ->
            Err "ValidationError: invalid minute"

        ( True, True ) ->
            let
                nextDay =
                    h < 7
            in
            Ok { hour = h, minute = m, nextDay = nextDay }


now : Task a DayTime
now =
    let
        convert : Time.Zone -> Posix -> DayTime
        convert zone posix =
            let
                hour =
                    Time.toHour zone posix

                minute =
                    Time.toMinute zone posix

                nextDay =
                    hour < 7
            in
            { hour = hour
            , minute = minute
            , nextDay = nextDay
            }
    in
    Task.map2
        convert
        Time.here
        Time.now


show : DayTime -> String
show { hour, minute } =
    let
        pad =
            String.fromInt >> String.padLeft 2 '0'
    in
    String.join ":" <| List.map pad [ hour, minute ]
