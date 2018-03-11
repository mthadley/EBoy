module Test.Util
    exposing
        ( ExpectState
        , Unit
        , expectByte
        , expectMem
        , expectMemWord
        , expectWord
        , toTest
        , withByte
        , withCode
        , withMem
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
        , init
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
    | ExpectMemWord Int Int


withCode : List Int -> Unit
withCode codes =
    let
        state =
            { init | memory = Memory.initFromInts codes }
    in
    { expectState = None
    , state = state
    , codes = codes
    }


and : ExpectState -> Unit -> Unit
and expectState unit =
    { unit
        | expectState =
            Batch
                [ unit.expectState
                , expectState
                ]
    }


withMem : Int -> Int -> Unit -> Unit
withMem loc val ({ state } as unit) =
    let
        newState =
            { state
                | memory =
                    Memory.writeByte
                        (Word.fromInt loc)
                        (Byte.fromInt val)
                        state.memory
            }
    in
    { unit | state = newState }


withByte : ByteRegister -> Int -> Unit -> Unit
withByte register val unit =
    { unit
        | state =
            writeByteRegister register
                unit.state
                (Byte.fromInt val)
    }


withWord : WordRegister -> Int -> Unit -> Unit
withWord register val unit =
    { unit
        | state =
            writeWordRegister register
                unit.state
                (Word.fromInt val)
    }


expectByte : ByteRegister -> Int -> Unit -> Unit
expectByte reg =
    and << ExpectByteRegister reg


expectMem : Int -> Int -> Unit -> Unit
expectMem loc =
    and << ExpectMem loc


expectMemWord : Int -> Int -> Unit -> Unit
expectMemWord loc =
    and << ExpectMemWord loc


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
            let
                actual =
                    readByteRegister register state
                        |> Byte.toInt
            in
            actual
                |> Expect.equal val
                |> onFail register val actual

        ExpectMemWord loc val ->
            let
                actual =
                    Memory.readWord (Word.fromInt loc) state.memory
                        |> Word.toInt
            in
            actual
                |> Expect.equal val
                |> onFail loc val actual

        ExpectMem loc val ->
            let
                actual =
                    Memory.readByte (Word.fromInt loc) state.memory
                        |> Byte.toInt
            in
            actual
                |> Expect.equal val
                |> onFail loc val actual

        ExpectWordRegister register val ->
            let
                actual =
                    readWordRegister register state
                        |> Word.toInt
            in
            actual
                |> Expect.equal val
                |> onFail register val actual


onFail : a -> b -> c -> Expectation -> Expectation
onFail loc val actual =
    Expect.onFail <|
        "Location "
            ++ toString loc
            ++ " should be "
            ++ toString val
            ++ " but was "
            ++ toString actual


toTest : Unit -> Test
toTest { codes, expectState, state } =
    (toExpectation expectState <| next state)
        |> always
        |> test ("Should match expected state: " ++ toString codes)
