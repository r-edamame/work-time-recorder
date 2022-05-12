module DayTime exposing (..)

import Parser exposing ((|.), (|=))
import Task exposing (Task)
import Time exposing (Posix)


type alias DayTime =
    { hour : Int
    , minute : Int
    , nextDay : Bool
    }


parseDayTime : String -> Result String DayTime
parseDayTime src =
    let
        parser =
            Parser.succeed (\h m -> ( h, m ))
                |= Parser.int
                |. Parser.symbol ":"
                |= Parser.int
    in
    case Parser.run parser src of
        Err deadends ->
            Err <| Parser.deadEndsToString deadends

        Ok ( h, m ) ->
            validateDayTime h m


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
