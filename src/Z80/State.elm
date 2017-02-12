module Z80.State exposing (..)

{-| Machine State and various functions that can be used to modify it.
-}

import Basics.Extra exposing ((=>))
import Byte exposing (Byte)
import Carry exposing (Carry)
import Memory exposing (Memory)
import Util
import Word exposing (Word)
import Z80.Cycles exposing (Cycles(..))
import Z80.Flag as Flag exposing (Flag)
import Z80.Mode as Mode exposing (Mode)
import Z80.Registers exposing (..)


{-| A record representing the current state of the CPU.
-}
type alias State =
    { clock : Int
    , memory : Memory
    , a : Byte
    , b : Byte
    , c : Byte
    , d : Byte
    , e : Byte
    , h : Byte
    , l : Byte
    , f : Byte
    , pc : Word {- 16-bit registers -}
    , sp : Word
    , mode : Mode
    , jump : Bool
    }


init : State
init =
    { clock = 0
    , memory = Memory.init
    , a = Byte.fromInt 0
    , b = Byte.fromInt 0
    , c = Byte.fromInt 0
    , d = Byte.fromInt 0
    , e = Byte.fromInt 0
    , h = Byte.fromInt 0
    , l = Byte.fromInt 0
    , f = Byte.fromInt 0
    , pc = Word.fromInt 0
    , sp = Word.fromInt 0
    , mode = Mode.Running
    , jump = False
    }


writeMemByte : Word -> Byte -> State -> State
writeMemByte addr byte state =
    { state | memory = Memory.writeByte addr byte state.memory }


writeMemWord : Word -> Word -> State -> State
writeMemWord addr word state =
    { state | memory = Memory.writeWord addr word state.memory }


writeMemRegister : WordRegister -> State -> Byte -> State
writeMemRegister wordRegister state byte =
    let
        addr =
            readWordRegister wordRegister state
    in
        writeMemByte addr byte state


writeByteRegister : ByteRegister -> State -> Byte -> State
writeByteRegister register state byte =
    case register of
        A ->
            { state | a = byte }

        B ->
            { state | b = byte }

        C ->
            { state | c = byte }

        D ->
            { state | d = byte }

        E ->
            { state | e = byte }

        H ->
            { state | h = byte }

        L ->
            { state | l = byte }

        F ->
            { state | f = byte }


writeWordRegister : WordRegister -> State -> Word -> State
writeWordRegister register state word =
    case register of
        PC ->
            { state | pc = word }

        SP ->
            { state | sp = word }

        BC ->
            let
                ( high, low ) =
                    Word.toBytes word
            in
                { state | b = high, c = low }

        HL ->
            let
                ( high, low ) =
                    Word.toBytes word
            in
                { state | h = high, l = low }

        DE ->
            let
                ( high, low ) =
                    Word.toBytes word
            in
                { state | d = high, e = low }

        AF ->
            let
                ( high, low ) =
                    Word.toBytes word
            in
                { state | d = high, e = low }


readDataWord : State -> ( Word, State )
readDataWord state =
    ( Memory.readWord state.pc state.memory
    , addPC 2 state
    )


readDataByte : State -> ( Byte, State )
readDataByte state =
    let
        newState =
            incPC state
    in
        ( Memory.readByte newState.pc newState.memory
        , newState
        )


readMemRegister : WordRegister -> State -> Byte
readMemRegister wordRegister state =
    Memory.readByte (readWordRegister wordRegister state) state.memory


readMemRegisterOffset : ByteRegister -> State -> Byte
readMemRegisterOffset register state =
    let
        addr =
            wordOffset <| readByteRegister register state
    in
        Memory.readByte addr state.memory


readMemDataOffset : State -> ( Byte, State )
readMemDataOffset state =
    let
        ( byte, newState ) =
            readDataByte state
    in
        ( Memory.readByte (wordOffset byte) state.memory
        , newState
        )


readWordRegister : WordRegister -> State -> Word
readWordRegister wordRegister state =
    case wordRegister of
        BC ->
            Word.fromBytes state.b state.c

        PC ->
            state.pc

        SP ->
            state.sp

        HL ->
            Word.fromBytes state.h state.l

        DE ->
            Word.fromBytes state.d state.e

        AF ->
            Word.fromBytes state.a state.f


readByteRegister : ByteRegister -> State -> Byte
readByteRegister register state =
    case register of
        A ->
            state.a

        B ->
            state.b

        C ->
            state.c

        D ->
            state.d

        E ->
            state.e

        F ->
            state.e

        H ->
            state.h

        L ->
            state.l


updateClock : Cycles -> State -> State
updateClock cycles state =
    case cycles of
        Always c ->
            { state | clock = state.clock + c }

        Branching taken notTaken ->
            let
                c =
                    if state.jump then
                        taken
                    else
                        notTaken
            in
                { state
                    | clock = state.clock + c
                    , jump = False
                }



-- PC


addPC : Int -> State -> State
addPC n state =
    { state | pc = Word.add state.pc <| Word.fromInt n }


addPCByte : Byte -> State -> State
addPCByte byte =
    addPC <| Byte.toInt byte


incPC : State -> State
incPC =
    addPC 1


wordOffset : Byte -> Word
wordOffset =
    Word.add (Word.fromInt 0xFF00) << Word.fromByte



-- Flags


setFlag : Flag -> State -> State
setFlag flags =
    updateFlags <| Flag.set flags


resetFlag : Flag -> State -> State
resetFlag flags =
    updateFlags <| Flag.reset flags


setFlags : List Flag -> State -> State
setFlags flags =
    updateFlags <| Flag.setEach flags


resetFlags : List Flag -> State -> State
resetFlags flags =
    updateFlags <| Flag.resetEach flags


setFlagsWith : List ( Flag, Bool ) -> State -> State
setFlagsWith flags =
    updateFlags <| Flag.setWith flags


setAccFlags : Carry Byte -> State -> State
setAccFlags result state =
    setCarryFlags result <|
        setFlagsWith
            [ Flag.Zero => (Byte.isZero <| Carry.value <| result)
            ]
            state


setCarryFlags : Carry a -> State -> State
setCarryFlags result =
    setFlagsWith
        [ Flag.Carry => Carry.check result
        , Flag.HalfCarry => Carry.checkHalf result
        ]


setIncFlags : Carry Byte -> State -> State
setIncFlags result =
    setFlagsWith
        [ Flag.HalfCarry => Carry.checkHalf result
        , Flag.Zero => (Byte.isZero <| Carry.value <| result)
        ]


getFlagByte : Flag -> State -> Byte
getFlagByte flag state =
    Flag.isSet flag state.f
        |> Util.toInt
        |> Byte.fromInt


updateFlags : (Byte -> Byte) -> State -> State
updateFlags func state =
    { state | f = func state.f }
