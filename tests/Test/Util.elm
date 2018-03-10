module Test.Util
    exposing
        ( ExpectState
        , Unit
        , expectByte
        , expectMem
        , expectWord
        , toTest
        , withByte
        , withCode
        , withWord
        )

import Byte
import Expect exposing (Expectation)
import Memory exposing (initFromInts)
import Test exposing (Test, test)
import Word
import Z80 exposing (next)
import Z80.Registers exposing (..)
import Z80.State as State
    exposing
        ( State
        , readByteRegister
        , readWordRegister
        , writeByteRegister
        , writeWordRegister
        )


type alias Unit =
    { expectState : ExpectState
    , state : State
    , codes : List Int
    }


type ExpectState
    = None
    | Batch (List ExpectState)
    | ExpectByteRegister ByteRegister Int
    | ExpectWordRegister WordRegister Int
    | ExpectMem Int Int


withCode : List Int -> Unit
withCode =
    Unit None State.init


and : ExpectState -> Unit -> Unit
and expectState unit =
    { unit
        | expectState =
            Batch
                [ unit.expectState
                , expectState
                ]
    }


withByte : ByteRegister -> Int -> Unit -> Unit
withByte register int unit =
    { unit
        | state =
            writeByteRegister register
                unit.state
                (Byte.fromInt int)
    }


withWord : WordRegister -> Int -> Unit -> Unit
withWord register int unit =
    { unit
        | state =
            writeWordRegister register
                unit.state
                (Word.fromInt int)
    }


expectByte : ByteRegister -> Int -> Unit -> Unit
expectByte reg =
    and << ExpectByteRegister reg


expectMem : Int -> Int -> Unit -> Unit
expectMem loc =
    and << ExpectMem loc


expectWord : WordRegister -> Int -> Unit -> Unit
expectWord reg =
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

        ExpectByteRegister register val ->
            readByteRegister register state
                |> Byte.toInt
                |> Expect.equal val
                |> registerFailure register val

        ExpectMem loc val ->
            Memory.readWord (Word.fromInt loc) state.memory
                |> Word.toInt
                |> Expect.equal val
                |> Expect.onFail
                    ("Memory location "
                        ++ toString loc
                        ++ " should be "
                        ++ toString val
                    )

        ExpectWordRegister register val ->
            readWordRegister register state
                |> Word.toInt
                |> Expect.equal val
                |> registerFailure register val


registerFailure : a -> Int -> Expectation -> Expectation
registerFailure register int =
    Expect.onFail <|
        "Register "
            ++ toString register
            ++ " should be "
            ++ toString int


toTest : Unit -> Test
toTest { codes, expectState, state } =
    let
        initState =
            { state | memory = initFromInts codes }
    in
    (toExpectation expectState <| next initState)
        |> always
        |> test ("Should match expected state: " ++ toString codes)
