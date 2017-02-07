module Z80 exposing (..)

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


{- Module handles advancing the state of the CPU.

   A few notes about the PC (Program Counter). The pc is updated as needed, as
   opposed to at the end when the instruction has finished processing. This mean's
   that the PC can be advanced (and state propagated) in `decodeIfCB`,
       `readData{Word|Byte}`, etc.
-}


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

        _ ->
            state



-- LW


readLWSource : LW.Source -> State -> ( Word, State )
readLWSource source state =
    case source of
        LW.FromRegister register ->
            ( readWordRegister register state, state )

        LW.FromData ->
            ( readDataWord state, addPC 2 state )

        LW.FromSPByteData ->
            let
                ( carry, halfCarry, result ) =
                    Word.addc
                        (readWordRegister SP state)
                        (Word.fromByte <| readDataByte state)
            in
                ( result
                , state
                    |> setFlagsWith
                        [ ( Flag.Carry, carry )
                        , ( Flag.HalfCarry, halfCarry )
                        ]
                    |> incPC
                )


writeLWTarget : LW.Target -> ( Word, State ) -> State
writeLWTarget target ( word, state ) =
    case target of
        LW.IntoRegister register ->
            writeWordRegister word register state

        LW.IntoMemData ->
            addPC 2 <| writeMemWord (readDataWord state) word state



-- LO


readLOSource : LO.Source -> State -> ( Byte, State )
readLOSource source state =
    case source of
        LO.FromRegisterA ->
            ( readByteRegister A state, state )

        LO.FromMemDataOffset ->
            ( readMemDataOffset state, incPC state )

        LO.FromMemCOffset ->
            ( readMemRegisterOffset C state, state )


writeLOTarget : LO.Target -> ( Byte, State ) -> State
writeLOTarget target ( byte, state ) =
    case target of
        LO.IntoRegisterA ->
            writeByteRegister byte A state

        LO.IntoMemDataOffset ->
            let
                word =
                    wordOffset <| readDataByte state
            in
                incPC <| writeMemByte byte word state

        LO.IntoMemCOffset ->
            let
                word =
                    wordOffset <| readByteRegister C state
            in
                writeMemByte byte word state



-- LB


readLBSource : LB.Source -> State -> ( Byte, State )
readLBSource source state =
    case source of
        LB.FromRegister register ->
            ( readByteRegister register state, state )

        LB.FromMem register ->
            ( readMemRegister register state, state )

        LB.FromData ->
            ( readDataByte state, incPC state )


writeLBTarget : LB.Target -> ( Byte, State ) -> State
writeLBTarget target ( byte, state ) =
    case target of
        LB.IntoRegister register ->
            writeByteRegister byte register state

        LB.IntoMem register ->
            writeMemRegister byte register state

        LB.IntoMemData ->
            addPC 2 <| writeMemByte byte (readDataWord state) state



-- Modifying State


writeMemByte : Byte -> Word -> State -> State
writeMemByte byte loc state =
    { state | memory = Memory.writeByte loc byte state.memory }


writeMemWord : Word -> Word -> State -> State
writeMemWord word loc state =
    { state | memory = Memory.writeWord loc word state.memory }


writeMemRegister : Byte -> WordRegister -> State -> State
writeMemRegister byte wordRegister state =
    let
        word =
            readWordRegister wordRegister state
    in
        writeMemByte byte word state


writeByteRegister : Byte -> ByteRegister -> State -> State
writeByteRegister byte register state =
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


readDataWord : State -> Word
readDataWord state =
    Word.fromBytes
        (readDataByte <| incPC state)
        (readDataByte state)


readDataByte : State -> Byte
readDataByte { memory, pc } =
    Memory.readByte (Word.inc pc) memory


readMemRegister : WordRegister -> State -> Byte
readMemRegister wordRegister state =
    Memory.readByte (readWordRegister wordRegister state) state.memory


readMemRegisterOffset : ByteRegister -> State -> Byte
readMemRegisterOffset register state =
    let
        word =
            wordOffset <| readByteRegister register state
    in
        Memory.readByte word state.memory


readMemDataOffset : State -> Byte
readMemDataOffset state =
    Memory.readByte (wordOffset <| readDataByte state) state.memory


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
