module TestDayTime exposing (..)

import DayTime exposing (DayTime, Period)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "DayTime module" <|
        let
            cases : List ( ( Int, Int ), ( Int, Int ), ( Int, Int ) )
            cases =
                [ ( ( 20, 40 ), ( 23, 30 ), ( 2, 50 ) )
                , ( ( 22, 15 ), ( 3, 30 ), ( 5, 15 ) )
                , ( ( 10, 0 ), ( 18, 30 ), ( 8, 30 ) )
                ]

            expects =
                cases
                    |> List.map
                        (\( ( fh, fm ), ( th, tm ), ( ph, pm ) ) ->
                            let
                                from =
                                    DayTime.validateDayTime fh fm

                                to =
                                    DayTime.validateDayTime th tm

                                period =
                                    Result.map2 DayTime.diff from to
                            in
                            Expect.equal period (Ok { hour = ph, minute = pm })
                        )
        in
        expects
            |> List.indexedMap
                (\ix expect ->
                    test ("DayTime diff " ++ String.fromInt ix) (\_ -> expect)
                )
