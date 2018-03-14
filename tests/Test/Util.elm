module Test.Util
    exposing
        ( ExpectState
        , Unit
        , expectByte
        , expectFlags
        , expectMem
        , expectMemWord
        , expectMode
        , expectWord
        , runTest
        , toExpectation
        , toTest
        , withByte
        , withCode
        , withMem
        , withWord
        )

import Byte exposing (Byte)
import Expect exposing (Expectation)
import Test exposing (Test, test)
import Word
import Z80 exposing (next)
import Z80.Flag as Flag exposing (Flag)
import Z80.MMU as MMU
import Z80.Mode exposing (Mode)
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
    | ExpectMode Mode
    | ExpectFlags (List ( Flag, Bool ))


withCode : List Int -> Unit
withCode codes =
    let
        mmu =
            codes
                |> List.map Byte.fromInt
                |> flip MMU.loadRom MMU.init
    in
    { expectState = None
    , state = { init | mmu = mmu }
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
                | mmu =
                    MMU.writeByte
                        (Word.fromInt loc)
                        (Byte.fromInt val)
                        state.mmu
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


expectFlags : List ( Flag, Bool ) -> Unit -> Unit
expectFlags =
    and << ExpectFlags


expectByte : ByteRegister -> Int -> Unit -> Unit
expectByte reg =
    and << ExpectByteRegister reg


expectMem : Int -> Int -> Unit -> Unit
expectMem loc =
    and << ExpectMem loc


expectMode : Mode -> Unit -> Unit
expectMode =
    and << ExpectMode


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

        ExpectMode mode ->
            Expect.equal mode <| state.mode

        ExpectFlags pairs ->
            pairs
                |> List.map (uncurry flagToExpectation)
                |> flip Expect.all state.f

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
                    MMU.readWord (Word.fromInt loc) state.mmu
                        |> Word.toInt
            in
            actual
                |> Expect.equal val
                |> onFail loc val actual

        ExpectMem loc val ->
            let
                actual =
                    MMU.readByte (Word.fromInt loc) state.mmu
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


flagToExpectation : Flag -> Bool -> Byte -> Expectation
flagToExpectation flag value =
    Flag.isSet flag
        >> Expect.equal value
        >> Expect.onFail
            ("Flag "
                ++ toString flag
                ++ " should be "
                ++ toString value
            )


onFail : a -> b -> c -> Expectation -> Expectation
onFail loc val actual =
    Expect.onFail <|
        "Location "
            ++ toString loc
            ++ " should be "
            ++ toString val
            ++ " but was "
            ++ toString actual


runTest : Unit -> () -> Expectation
runTest { codes, expectState, state } =
    (toExpectation expectState <| next state)
        |> always


toTest : Unit -> Test
toTest unit =
    runTest unit
        |> test ("Should match expected state: " ++ toString unit.codes)
