module Main exposing (..)

import Byte exposing (Byte)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Time
import Word exposing (Word)
import Z80
import Z80.Decode exposing (decode)
import Z80.Registers exposing (..)
import Z80.State as State exposing (State)


-- Model


type alias Model =
    { state : State
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    Model State.init False ! []



-- View


view : Model -> Html Msg
view model =
    main_ []
        [ viewRegisterTable model.state
        , viewStateTable model.state
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Next ] [ text "Next" ]
        , div []
            [ button [ onClick ToggleRun ]
                [ if model.running then
                    text "Halt"
                  else
                    text "Run"
                ]
            ]
        ]


viewStateTable : State -> Html msg
viewStateTable state =
    table []
        [ tr []
            [ th [] [ text "Clock" ]
            , td [] [ text <| toString <| state.clock ]
            ]
        , tr []
            [ th [] [ text "Instruction" ]
            , td [] [ viewInstruction state ]
            ]
        ]


viewInstruction : State -> Html msg
viewInstruction state =
    state
        |> State.readMemRegister PC
        |> decode
        |> Tuple.first
        |> toString
        |> text


viewRegisterTable : State -> Html msg
viewRegisterTable state =
    table []
        [ thead []
            [ th [] [ text "Registers" ]
            ]
        , tbody []
            [ tr []
                [ th [] [ text "A" ]
                , td [] [ viewByte state.a ]
                , th [] [ text "F" ]
                , td [] [ viewByte state.f ]
                ]
            , tr []
                [ th [] [ text "B" ]
                , td [] [ viewByte state.b ]
                , th [] [ text "C" ]
                , td [] [ viewByte state.c ]
                ]
            , tr []
                [ th [] [ text "D" ]
                , td [] [ viewByte state.d ]
                , th [] [ text "E" ]
                , td [] [ viewByte state.e ]
                ]
            , tr []
                [ th [] [ text "H" ]
                , td [] [ viewByte state.h ]
                , th [] [ text "L" ]
                , td [] [ viewByte state.l ]
                ]
            , tr []
                [ th [] [ text "PC" ]
                , td [ Attr.colspan 3 ] [ viewWord state.pc ]
                ]
            , tr []
                [ th [] [ text "SP" ]
                , td [ Attr.colspan 3 ] [ viewWord state.sp ]
                ]
            ]
        ]


viewByte : Byte -> Html msg
viewByte =
    text << intToHex 2 << Byte.toInt


viewWord : Word -> Html msg
viewWord =
    text << intToHex 4 << Word.toInt


intToHex : Int -> Int -> String
intToHex pad int =
    let
        helper int str =
            if int == 0 then
                String.padLeft pad '0' str
            else
                helper (int // 16) <| (digitToHex <| int % 16) ++ str
    in
    helper int ""


digitToHex : Int -> String
digitToHex int =
    if int < 10 then
        toString int
    else
        case int of
            10 ->
                "A"

            11 ->
                "B"

            12 ->
                "C"

            13 ->
                "D"

            14 ->
                "E"

            15 ->
                "F"

            _ ->
                "G"



-- Update


type Msg
    = Reset
    | Next
    | Run
    | ToggleRun


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            { model | state = State.init } ! []

        Next ->
            { model | state = Z80.next model.state } ! []

        ToggleRun ->
            { model | running = not model.running } ! []

        Run ->
            { model | state = run model.state } ! []


run : State -> State
run state =
    if state.clock < 17556 then
        run <| Z80.next state
    else
        { state | clock = 0 }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every Time.millisecond (always Run)
    else
        Sub.none



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
