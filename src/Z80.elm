module Z80 exposing (..)

import Byte exposing (Byte)
import Memory exposing (Memory)
import Word exposing (Word)
import Z80.Decode as Decode exposing (Cycles(..))
import Z80.Flag as Flag
import Z80.LB as LB
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
        ( op, cycles ) =
            decodeIfCB state <| Decode.decode byte
    in
        ( op
        , cycles
        , state
        )


decodeIfCB : State -> ( Op, Cycles ) -> ( Op, Cycles )
decodeIfCB state ( op, cycles ) =
    if Decode.isCB op then
        fetch (incPC state)
            |> Tuple.first
            |> Decode.decodeCB
    else
        ( op, cycles )


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
            state

        NOP ->
            state

        LD target source ->
            writeLBTarget target <| readLBSource source state

        _ ->
            state



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
            writeRegisterByte byte register state

        LB.IntoMem register ->
            writeMemRegister byte register state

        LB.IntoMemData ->
            incPC <| writeMemWord byte (readDataWord state) state



-- Modifying State


writeMemWord : Byte -> Word -> State -> State
writeMemWord byte word state =
    { state | memory = Memory.writeByte word byte state.memory }


writeMemRegister : Byte -> WordRegister -> State -> State
writeMemRegister byte wordRegister state =
    let
        word =
            readWordRegister wordRegister state
    in
        writeMemWord byte word state


writeRegisterByte : Byte -> ByteRegister -> State -> State
writeRegisterByte byte register state =
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
