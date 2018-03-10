module Test.Z80 exposing (..)

import Basics.Extra exposing ((=>))
import Byte
import Expect exposing (Expectation)
import Memory exposing (initFromInts)
import Test exposing (..)
import Word
import Z80 exposing (next)
import Z80.Registers exposing (..)
import Z80.State
    exposing
        ( State
        , init
        , readByteRegister
        , readWordRegister
        )


states : List Unit
states =
    [ expect 0x00
        |> byte A 0
        |> byte B 0
        |> byte C 0
        |> byte D 0
        |> byte E 0
        |> byte H 0
        |> byte L 0
        |> byte F 0
        |> word PC 1
        |> word SP 0xFFFE
        |> word BC 0
        |> word HL 0
        |> word DE 0
        |> word AF 0
    , expect 0x03
        |> byte B 0
        |> byte C 1
        |> word BC 1
    , expect 0x04
        |> byte B 1
    , expect 0x05
        |> byte B 255
    ]


type alias Unit =
    { expectState : ExpectState
    , code : Int
    }


type ExpectState
    = None
    | Batch (List ExpectState)
    | ExpectByteRegister ByteRegister Int
    | ExpectWordRegister WordRegister Int


expect : Int -> Unit
expect =
    Unit None


and : ExpectState -> Unit -> Unit
and expectState unit =
    { unit
        | expectState =
            Batch
                [ unit.expectState
                , expectState
                ]
    }


byte : ByteRegister -> Int -> Unit -> Unit
byte reg =
    and << ExpectByteRegister reg


word : WordRegister -> Int -> Unit -> Unit
word reg =
    and << ExpectWordRegister reg


toExpectation : ExpectState -> State -> Expectation
toExpectation expectState state =
    case expectState of
        None ->
            Expect.pass

        Batch expects ->
            expects
                |> List.map toExpectation
                |> flip Expect.all state

        ExpectByteRegister register int ->
            readByteRegister register state
                |> Byte.toInt
                |> Expect.equal int
                |> registerFailure register int

        ExpectWordRegister register int ->
            readWordRegister register state
                |> Word.toInt
                |> Expect.equal int
                |> registerFailure register int


registerFailure : a -> Int -> Expectation -> Expectation
registerFailure register int =
    Expect.onFail <|
        "Register "
            ++ toString register
            ++ " should be "
            ++ toString int


expectNextState : Unit -> Test
expectNextState { code, expectState } =
    let
        initState =
            { init | memory = initFromInts [ code ] }
    in
    test ("Should match expected state: " ++ toString code) <|
        \_ -> toExpectation expectState <| next initState


suite : Test
suite =
    states
        |> List.map expectNextState
        |> describe "Z80"
