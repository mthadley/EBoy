module Z80.State exposing
    ( State
    , addByteSigned
    , addPC
    , addPCSigned
    , addSP
    , decSP
    , enableInterrupts
    , getFlagByte
    , incPC
    , incSP
    , init
    , popSP
    , pushSP
    , pushSPWord
    , readByteRegister
    , readDataByte
    , readDataWord
    , readMemDataOffset
    , readMemRegister
    , readMemRegisterOffset
    , readMemWordRegister
    , readWordRegister
    , resetFlag
    , resetFlags
    , setAccFlags
    , setCarryFlags
    , setFlag
    , setFlags
    , setFlagsWith
    , setIncFlags
    , setZeroFlag
    , updateClock
    , updateFlags
    , wordOffset
    , writeByteRegister
    , writeMemByte
    , writeMemRegister
    , writeMemRegisterWord
    , writeMemWord
    , writeWordRegister
    )

{-| Machine State and various functions that can be used to modify it.
-}

import Byte exposing (Byte)
import Carry exposing (Carry)
import Util
import Word exposing (Word)
import Z80.Cycles exposing (Cycles(..))
import Z80.Flag as Flag exposing (Flag)
import Z80.MMU as MMU exposing (MMU)
import Z80.Mode as Mode exposing (Mode)
import Z80.Registers exposing (..)


{-| A record representing the current state of the CPU.
-}
type alias State =
    { clock : Int
    , mmu : MMU
    , a : Byte
    , b : Byte
    , c : Byte
    , d : Byte
    , e : Byte
    , h : Byte
    , l : Byte
    , f : Byte
    , pc : Word

    {- 16-bit registers -}
    , sp : Word
    , mode : Mode
    , jump : Bool
    , interrupts : Bool
    }


init : State
init =
    { clock = 0
    , mmu = MMU.init
    , a = Byte.fromInt 0
    , b = Byte.fromInt 0
    , c = Byte.fromInt 0
    , d = Byte.fromInt 0
    , e = Byte.fromInt 0
    , h = Byte.fromInt 0
    , l = Byte.fromInt 0
    , f = Byte.fromInt 0
    , pc = Word.fromInt 0
    , sp = Word.fromInt 0xFFFE
    , mode = Mode.Running
    , jump = False
    , interrupts = True
    }


writeMemByte : Word -> Byte -> State -> State
writeMemByte addr byte state =
    { state | mmu = MMU.writeByte addr byte state.mmu }


writeMemWord : Word -> Word -> State -> State
writeMemWord addr word state =
    { state | mmu = MMU.writeWord addr word state.mmu }


writeMemRegister : WordRegister -> State -> Byte -> State
writeMemRegister register state byte =
    writeMemByte (readWordRegister register state) byte state


writeMemRegisterWord : WordRegister -> State -> Word -> State
writeMemRegisterWord register state word =
    writeMemWord (readWordRegister register state) word state


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
    let
        ( high, low ) =
            Word.toBytes word
    in
    case register of
        PC ->
            { state | pc = word }

        SP ->
            { state | sp = word }

        BC ->
            { state | b = high, c = low }

        HL ->
            { state | h = high, l = low }

        DE ->
            { state | d = high, e = low }

        AF ->
            { state | d = high, e = low }


readDataWord : State -> ( Word, State )
readDataWord state =
    let
        newState =
            incPC state
    in
    ( MMU.readWord newState.pc state.mmu
    , incPC newState
    )


readDataByte : State -> ( Byte, State )
readDataByte state =
    let
        newState =
            incPC state
    in
    ( MMU.readByte newState.pc newState.mmu
    , newState
    )


readMemRegister : WordRegister -> State -> Byte
readMemRegister wordRegister state =
    MMU.readByte (readWordRegister wordRegister state) state.mmu


readMemWordRegister : WordRegister -> State -> Word
readMemWordRegister register state =
    MMU.readWord state.sp state.mmu


readMemRegisterOffset : ByteRegister -> State -> Byte
readMemRegisterOffset register state =
    let
        addr =
            wordOffset <| readByteRegister register state
    in
    MMU.readByte addr state.mmu


readMemDataOffset : State -> ( Byte, State )
readMemDataOffset state =
    let
        ( byte, newState ) =
            readDataByte state
    in
    ( MMU.readByte (wordOffset byte) state.mmu
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


addPCSigned : Byte -> State -> State
addPCSigned byte state =
    { state | pc = addByteSigned byte state.pc }


addByteSigned : Byte -> Word -> Word
addByteSigned byte =
    let
        sign =
            if Byte.msbSet byte then
                -1

            else
                1

        val =
            byte
                |> Byte.reset 7
                |> Byte.toInt
                |> (*) sign
    in
    Word.map ((+) val)


incPC : State -> State
incPC =
    addPC 1


wordOffset : Byte -> Word
wordOffset =
    Word.add (Word.fromInt 0xFF00) << Word.fromByte



-- Stack Pointer


addSP : Int -> State -> State
addSP n state =
    { state | sp = Word.add state.sp <| Word.fromInt n }


decSP : State -> State
decSP =
    addSP -2


incSP : State -> State
incSP =
    addSP 2


pushSP : WordRegister -> State -> State
pushSP register state =
    readWordRegister register state
        |> writeMemRegisterWord SP (decSP state)


pushSPWord : State -> Word -> State
pushSPWord state word =
    writeMemRegisterWord SP (decSP state) word


popSP : WordRegister -> State -> State
popSP register state =
    readMemWordRegister SP state
        |> writeWordRegister register state
        |> incSP



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


setAccFlags : ( Carry Byte, State ) -> State
setAccFlags ( result, state ) =
    setCarryFlags result <|
        setFlagsWith
            [ ( Flag.Zero, Byte.isZero <| Carry.value <| result ) ]
            state


setCarryFlags : Carry a -> State -> State
setCarryFlags result =
    setFlagsWith
        [ ( Flag.Carry, Carry.check result )
        , ( Flag.HalfCarry, Carry.checkHalf result )
        ]


setIncFlags : Carry Byte -> State -> State
setIncFlags result =
    setFlagsWith
        [ ( Flag.HalfCarry, Carry.checkHalf result )
        , ( Flag.Zero, Byte.isZero <| Carry.value <| result )
        ]


setZeroFlag : Byte -> State -> State
setZeroFlag byte =
    setFlagsWith
        [ ( Flag.Zero, Byte.isZero byte )
        ]


getFlagByte : Flag -> State -> Byte
getFlagByte flag state =
    Flag.isSet flag state.f
        |> Util.toInt
        |> Byte.fromInt


updateFlags : (Byte -> Byte) -> State -> State
updateFlags func state =
    { state | f = func state.f }



-- Interrupts


enableInterrupts : Bool -> State -> State
enableInterrupts interrupts state =
    { state | interrupts = interrupts }
