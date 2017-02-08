module Z80.Update exposing (..)

import Z80.Flag as Flag exposing (Flag)
import Z80.Decode exposing (Cycles(..))


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


resetFlags : List Flag -> State -> State
resetFlags flags =
    updateFlags <| Flag.reset flags


setFlagsWith : List ( Flag, Bool ) -> State -> State
setFlagsWith flags =
    updateFlags <| Flag.setWith flags


updateFlags : (Byte -> Byte) -> State -> State
updateFlags func state =
    { state | f = func state.f }
