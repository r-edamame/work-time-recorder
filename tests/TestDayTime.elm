module TestDayTime exposing (..)

import DayTime exposing (DayTime, Period)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "DayTime module"
        [ describe "DayTime.diff" <|
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
        , describe "DayTime.addPeriod" <|
            let
                cases : List ( ( Int, Int ), ( Int, Int ), ( Int, Int ) )
                cases =
                    [ ( ( 1, 30 ), ( 0, 30 ), ( 2, 0 ) )
                    , ( ( 3, 0 ), ( 0, 50 ), ( 3, 50 ) )
                    ]

                expects =
                    cases
                        |> List.map
                            (\( ( p1h, p1m ), ( p2h, p2m ), ( sh, sm ) ) ->
                                let
                                    sum =
                                        DayTime.addPeriod { hour = p1h, minute = p1m } { hour = p2h, minute = p2m }
                                in
                                Expect.equal sum { hour = sh, minute = sm }
                            )
            in
            expects
                |> List.indexedMap
                    (\ix expect ->
                        test ("DayTime addPeriod " ++ String.fromInt ix) (\_ -> expect)
                    )
        ]
