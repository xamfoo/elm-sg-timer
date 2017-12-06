port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Time exposing (Time, every, second)


type alias Model =
    { numberOfParticipants : Int
    , timePerParticipant : Int
    , totalTime : Int
    , remainingTotalTime : Int
    , isPaused : Bool
    , page : Page
    }


model : Model
model =
    Model 0 0 0 0 False Setup


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.page == Running && model.remainingTotalTime > 0 && not model.isPaused then
        every second Tick
    else
        Sub.none


type Page
    = Setup
    | Ready
    | Running


type Msg
    = UpdateParticipants String
    | UpdateTotalTime String
    | MsgReset
    | MsgReady
    | Start
    | Tick Time
    | TogglePause
    | Ring


port ring : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateParticipants num ->
            let
                finalNum =
                    case String.toInt num of
                        Ok val ->
                            val

                        Err _ ->
                            0
            in
                ( { model | numberOfParticipants = finalNum }, Cmd.none )

        MsgReady ->
            ( { model | page = Ready }, Cmd.none )

        MsgReset ->
            ( { model | page = Setup }, Cmd.none )

        Start ->
            ( { model | page = Running }, Cmd.none )

        Tick _ ->
            ( { model | remainingTotalTime = model.remainingTotalTime - 1 }, Cmd.none )

        UpdateTotalTime time ->
            let
                finalNum =
                    floor
                        (case String.toFloat time of
                            Ok val ->
                                val * 60

                            Err _ ->
                                0
                        )
            in
                ( { model | totalTime = finalNum, remainingTotalTime = finalNum }, Cmd.none )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )

        Ring ->
            ( model, ring () )


viewHeader =
    div [ class "header" ]
        [ h1 [] [ text "Elm Singapore" ]
        , h2 [] [ text "TIMER" ]
        ]


viewInputs =
    div [ class "content" ]
        [ div [ class "ready-input" ]
            [ input
                [ placeholder "Total Time"
                , onEnter MsgReady
                , onInput UpdateTotalTime
                ]
                []
            , span [] [ text "minutes" ]
            ]
        ]


viewReadyButtons =
    div [ class "content-input" ]
        [ div []
            [ button [ onClick MsgReset ] [ text "RESET" ]
            , button [ onClick Start ] [ text "START" ]
            ]
        ]


viewSetupButtons =
    div [ class "content-input" ]
        [ div []
            [ button [] [ text "RESET" ]
            , button [ onClick MsgReady ] [ text "READY" ]
            ]
        ]


viewCalculatedValues participantSeconds =
    div [ class "content view-calculated" ]
        [ p [] [ (participantSeconds |> toString |> (++)) " seconds " |> text ]
        , p [] [ text "per participant" ]
        ]


viewCountdown : Int -> Int -> Html Msg
viewCountdown remainingTime remainingParticipants =
    let
        remainingMins =
            remainingTime // 60

        remainingSecs =
            remainingTime % 60
    in
        div
            [ class "content view-countdown" ]
            [ div []
                [ span [] [ text (String.padLeft 2 '0' (toString remainingMins)) ]
                , span [] [ text ":" ]
                , span [] [ text (String.padLeft 2 '0' (toString remainingSecs)) ]
                ]
            , div [] [ text ((toString remainingParticipants) ++ " participants left") ]
            ]


viewCountdownButtons : Int -> Bool -> Html Msg
viewCountdownButtons remainingTotalTime isPaused =
    div [ class "content-input" ]
        [ button [ onClick MsgReset ]
            [ text "Reset" ]
        , if remainingTotalTime > 0 then
            button [ onClick TogglePause ]
                [ text
                    (if isPaused then
                        "Resume"
                     else
                        "Pause"
                    )
                ]
          else
            text ""
        ]


view : Model -> Html Msg
view model =
    let
        timePerParticipant =
            (toFloat model.totalTime) / (toFloat model.numberOfParticipants)

        remainingParticipants =
            floor ((toFloat model.remainingTotalTime) / timePerParticipant)

        remainingTime =
            floor ((toFloat model.remainingTotalTime) - (toFloat remainingParticipants) * timePerParticipant)

        body =
            case model.page of
                Setup ->
                    [ viewInputs, viewSetupButtons ]

                Ready ->
                    [ viewCalculatedValues (floor timePerParticipant)
                    , viewReadyButtons
                    ]

                Running ->
                    [ viewCountdown remainingTime remainingParticipants
                    , viewCountdownButtons model.remainingTotalTime model.isPaused
                    ]
    in
        div [ class "root" ]
            [ viewHeader
            , div
                [ class "main"
                , classList [ ( "has-alert", remainingTime <= 3 && remainingTime > 0 ) ]
                ]
                body
            ]


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
