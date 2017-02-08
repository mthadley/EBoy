module Z80 exposing (..)

import Basics.Extra exposing (..)
import Byte exposing (Byte)
import Memory exposing (Memory)
import Word exposing (Word)
import Z80.Decode as Decode exposing (Cycles(..))
import Z80.Flag as Flag exposing (Flag)
import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Op exposing (..)
import Z80.Registers exposing (..)
import Z80.State as State exposing (State)


next : State -> State
next =
    execute << decode << fetch


fetch : State -> ( Byte, State )
fetch state =
    ( Memory.readByte state.pc state.memory
    , state
    )


decode : ( Byte, State ) -> ( Op, Cycles, State )
decode ( byte, state ) =
    let
        ( op, cycles, newState ) =
            decodeIfCB state <| Decode.decode byte
    in
        ( op
        , cycles
        , newState
        )


decodeIfCB : State -> ( Op, Cycles ) -> ( Op, Cycles, State )
decodeIfCB state ( op, cycles ) =
    if Decode.isCB op then
        let
            newState =
                incPC state

            ( newOp, newCycles ) =
                fetch newState
                    |> Tuple.first
                    |> Decode.decodeCB
        in
            ( newOp, newCycles, newState )
    else
        ( op, cycles, state )


execute : ( Op, Cycles, State ) -> State
execute ( op, cycles, state ) =
    executeOp op state
        |> updateClock cycles


executeOp : Op -> State -> State
executeOp op state =
    case op of
        INVALID x ->
            Debug.crash <| toString x

        NONE ->
            incPC <| state

        NOP ->
            incPC <| state

        LD target source ->
            readLBSource source state
                |> writeLBTarget target
                |> incPC

        LDH target source ->
            readLOSource source state
                |> writeLOTarget target
                |> incPC

        LDW target source ->
            readLWSource source state
                |> writeLWTarget target
                |> incPC

        INC param ->
            let
                ( result, newState ) =
                    applyParamWith Byte.incc param state
            in
                newState
                    |> setFlagsWith
                        [ Flag.Carry => False
                        , Flag.HalfCarry => Byte.hasHalfCarry result
                        , Flag.Zero => (Byte.isZero <| Byte.resultToByte <| result)
                        ]
                    |> incPC

        _ ->
            state


applyParamWith : (Byte -> Byte.Result) -> Param -> State -> ( Byte.Result, State )
applyParamWith f param state =
    case param of
        OnRegister register ->
            let
                result =
                    f <| readByteRegister register state
            in
                ( result
                , writeByteRegister register state <| Byte.resultToByte result
                )

        OnMemHL ->
            let
                result =
                    f <| readMemRegister HL state
            in
                ( result
                , writeMemRegister HL state <| Byte.resultToByte result
                )



-- LW


readLWSource : LW.Source -> State -> ( Word, State )
readLWSource source state =
    case source of
        LW.FromRegister register ->
            ( readWordRegister register state, state )

        LW.FromData ->
            readDataWord state

        LW.FromSPByteData ->
            let
                ( byte, newState ) =
                    readDataByte state

                result =
                    Word.addc
                        (readWordRegister SP state)
                        (Word.fromByte byte)
            in
                ( Word.resultToWord result
                , setFlagsWith
                    [ Flag.Carry => Word.hasCarry result
                    , Flag.HalfCarry => Word.hasHalfCarry result
                    ]
                    state
                )


writeLWTarget : LW.Target -> ( Word, State ) -> State
writeLWTarget target ( word, state ) =
    case target of
        LW.IntoRegister register ->
            writeWordRegister word register state

        LW.IntoMemData ->
            let
                ( addr, newState ) =
                    readDataWord state
            in
                writeMemWord addr word newState



-- LO


readLOSource : LO.Source -> State -> ( Byte, State )
readLOSource source state =
    case source of
        LO.FromRegisterA ->
            ( readByteRegister A state, state )

        LO.FromMemDataOffset ->
            readMemDataOffset state

        LO.FromMemCOffset ->
            ( readMemRegisterOffset C state, state )


writeLOTarget : LO.Target -> ( Byte, State ) -> State
writeLOTarget target ( byte, state ) =
    case target of
        LO.IntoRegisterA ->
            writeByteRegister A state byte

        LO.IntoMemDataOffset ->
            let
                ( word, newState ) =
                    Tuple.mapFirst wordOffset <| readDataByte state
            in
                writeMemByte word byte newState

        LO.IntoMemCOffset ->
            let
                addr =
                    wordOffset <| readByteRegister C state
            in
                writeMemByte addr byte state



-- LB


readLBSource : LB.Source -> State -> ( Byte, State )
readLBSource source state =
    case source of
        LB.FromRegister register ->
            ( readByteRegister register state, state )

        LB.FromMem register ->
            ( readMemRegister register state, state )

        LB.FromData ->
            readDataByte state


writeLBTarget : LB.Target -> ( Byte, State ) -> State
writeLBTarget target ( byte, state ) =
    case target of
        LB.IntoRegister register ->
            writeByteRegister register state byte

        LB.IntoMem register ->
            writeMemRegister register state byte

        LB.IntoMemData ->
            let
                ( addr, newState ) =
                    readDataWord state
            in
                writeMemByte addr byte newState



-- Modifying State


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


writeWordRegister : Word -> WordRegister -> State -> State
writeWordRegister word register state =
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
    let
        count =
            case cycles of
                Always c ->
                    c

                Branching taken notTaken ->
                    taken
    in
        { state | clock = state.clock + count }


addPC : Int -> State -> State
addPC n state =
    { state | pc = Word.add state.pc <| Word.fromInt n }


incPC : State -> State
incPC =
    addPC 1


wordOffset : Byte -> Word
wordOffset =
    Word.add (Word.fromInt 0xFF00) << Word.fromByte


setFlagsWith : List ( Flag, Bool ) -> State -> State
setFlagsWith flags =
    updateFlags <| Flag.setWith flags


updateFlags : (Byte -> Byte) -> State -> State
updateFlags func state =
    { state | f = func state.f }
