module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task exposing (perform)
import Time exposing (Posix)



-- Model


type alias Model =
    { log : List Posix
    , zone : Maybe Time.Zone
    }



-- Msg


type Msg
    = AddLogReq
    | AddLog Posix
    | ReceiveZone Time.Zone


init : a -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { log = []
            , zone = Nothing
            }
    in
    ( model, perform ReceiveZone Time.here )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLogReq ->
            ( model, perform AddLog Time.now )

        AddLog time ->
            ( { model | log = time :: model.log }, Cmd.none )

        ReceiveZone zone ->
            ( { model | zone = Just zone }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        logView =
            case model.zone of
                Nothing ->
                    []

                Just zone ->
                    List.map (viewTimeLog zone) model.log
    in
    div []
        [ button [ onClick AddLogReq ] [ text "save" ]
        , div [] logView
        ]


viewTimeLog : Time.Zone -> Posix -> Html Msg
viewTimeLog zone log =
    div [] [ text (formatPosix zone log) ]


formatPosix : Time.Zone -> Posix -> String
formatPosix zone posix =
    let
        toStringWithPad : Int -> String
        toStringWithPad =
            String.fromInt >> String.padLeft 2 '0'

        hour =
            toStringWithPad <| Time.toHour zone posix

        minute =
            toStringWithPad <| Time.toMinute zone posix

        second =
            toStringWithPad <| Time.toSecond zone posix
    in
    String.join ":" [ hour, minute, second ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
