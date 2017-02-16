module Z80 exposing (next)

import Basics.Extra exposing ((=>))
import Byte exposing (Byte)
import Carry exposing (Carry)
import Memory exposing (Memory)
import Util
import Word exposing (Word)
import Z80.Cycles exposing (Cycles)
import Z80.Decode as Decode
import Z80.Flag as Flag exposing (Flag)
import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Mode as Mode
import Z80.Op exposing (..)
import Z80.Registers exposing (..)
import Z80.State as State exposing (..)


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
            LB.readSource source state
                |> LB.writeTarget target
                |> incPC

        LDH target source ->
            LO.readSource source state
                |> LO.writeTarget target
                |> incPC

        LDW target source ->
            LW.readSource source state
                |> LW.writeTarget target
                |> incPC

        INC param ->
            applyWith Byte.incc param state
                |> uncurry setIncFlags
                |> resetFlag Flag.Subtract
                |> incPC

        INCW register ->
            incPC <| applyWordWith Word.inc register state

        DEC param ->
            applyWith Byte.decc param state
                |> uncurry setIncFlags
                |> setFlag Flag.Subtract
                |> incPC

        DECW register ->
            incPC <| applyWordWith Word.dec register state

        ADD param ->
            accCarryWith Byte.addc param state
                |> uncurry setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADC param ->
            accumulateCarryWith Byte.addc param state
                |> uncurry setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADDW register ->
            readWordRegister register state
                |> Word.addc (readWordRegister HL state)
                |> Util.clone
                |> Tuple.mapSecond (writeWordRegister register state << Carry.value)
                |> uncurry setCarryFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADDSP ->
            let
                ( byte, newState ) =
                    readDataByte state
            in
                Word.addc (readWordRegister SP newState) (Word.fromByte byte)
                    |> Util.clone
                    |> Tuple.mapSecond (writeWordRegister SP newState << Carry.value)
                    |> uncurry setCarryFlags
                    |> resetFlags [ Flag.Zero, Flag.Subtract ]
                    |> incPC

        RRCA ->
            let
                byte =
                    readByteRegister A state
            in
                byte
                    |> Byte.rotateRight
                    |> writeByteRegister A state
                    |> resetFlags [ Flag.Zero, Flag.Subtract, Flag.HalfCarry ]
                    |> setFlagsWith [ Flag.Carry => Byte.lsbSet byte ]
                    |> incPC

        STOP ->
            incPC <| { state | mode = Mode.Stopped }

        RLA ->
            rotateA Left state

        JR condition ->
            if shouldJump condition state then
                readDataByte state
                    |> uncurry addPCByte
                    |> setJump True
                    |> incPC
            else
                readDataByte state
                    |> Tuple.second
                    |> setJump False
                    |> incPC

        RRA ->
            rotateA Right state

        LDI target source ->
            LB.readSource source state
                |> LB.writeTarget target
                |> applyWordWith Word.inc HL
                |> incPC

        DAA ->
            let
                subtract =
                    Flag.isSet Flag.Subtract state.f
            in
                readByteRegister A state
                    |> bcdCorrect Low (Flag.isSet Flag.HalfCarry state.f) subtract
                    |> Carry.value
                    |> bcdCorrect High (Flag.isSet Flag.Carry state.f) subtract
                    |> Util.clone
                    |> Tuple.mapSecond (writeByteRegister A state << Carry.value)
                    |> uncurry setAccFlags
                    |> resetFlag Flag.HalfCarry
                    |> incPC

        CPL ->
            readByteRegister A state
                |> Byte.complement
                |> writeByteRegister A state
                |> resetFlags [ Flag.HalfCarry, Flag.Subtract ]
                |> incPC

        LDD target source ->
            LB.readSource source state
                |> LB.writeTarget target
                |> applyWordWith Word.dec HL
                |> incPC

        SCF ->
            state
                |> setFlag Flag.Carry
                |> resetFlags [ Flag.HalfCarry, Flag.Subtract ]
                |> incPC

        CCF ->
            state
                |> setFlagsWith [ Flag.Carry => (not <| Flag.isSet Flag.Carry state.f) ]
                |> resetFlags [ Flag.HalfCarry, Flag.Subtract ]
                |> incPC

        HALT ->
            incPC <| { state | mode = Mode.Halted }

        SUB param ->
            accCarryWith Byte.subc param state
                |> uncurry setAccFlags
                |> setFlag Flag.Subtract
                |> incPC

        SBC param ->
            accumulateCarryWith Byte.subc param state
                |> uncurry setAccFlags
                |> setFlag Flag.Subtract
                |> incPC

        AND param ->
            accByteWith Byte.and param state
                |> uncurry setZeroFlag
                |> resetFlags [ Flag.Subtract, Flag.Carry ]
                |> setFlag Flag.HalfCarry
                |> incPC

        OR param ->
            accByteWith Byte.or param state
                |> uncurry setZeroFlag
                |> resetFlags [ Flag.Subtract, Flag.Carry, Flag.HalfCarry ]
                |> incPC

        _ ->
            state


accByteWith : (Byte -> Byte -> Byte) -> ParamData -> State -> ( Byte, State )
accByteWith =
    accumulateWith identity


accCarryWith : (Byte -> Byte -> Carry Byte) -> ParamData -> State -> ( Carry Byte, State )
accCarryWith =
    accumulateWith Carry.value


accumulateWith :
    (a -> Byte)
    -> (Byte -> Byte -> a)
    -> ParamData
    -> State
    -> ( a, State )
accumulateWith f g param state =
    readParamData param state
        |> Tuple.mapFirst (g <| readByteRegister A state)
        |> writeAccumulator f


accumulateCarryWith :
    (Byte -> Byte -> Carry Byte)
    -> ParamData
    -> State
    -> ( Carry Byte, State )
accumulateCarryWith f param state =
    let
        operand =
            Byte.add
                (readByteRegister A state)
                (getFlagByte Flag.Carry state)
    in
        readParamData param state
            |> Tuple.mapFirst (f operand)
            |> writeAccumulator Carry.value


readParamData : ParamData -> State -> ( Byte, State )
readParamData param state =
    case param of
        WithRegister register ->
            ( readByteRegister register state, state )

        WithMemHL ->
            ( readMemRegister HL state, state )

        WithData ->
            readDataByte state


writeAccumulator : (a -> Byte) -> ( a, State ) -> ( a, State )
writeAccumulator f ( result, state ) =
    ( result
    , writeByteRegister A state <| f result
    )


applyWith :
    (Byte -> Carry Byte)
    -> Param
    -> State
    -> ( Carry Byte, State )
applyWith f param state =
    case param of
        OnRegister register ->
            readByteRegister register state
                |> Util.cloneWith f
                |> Tuple.mapSecond (writeByteRegister register state << Carry.value)

        OnMemHL ->
            readMemRegister HL state
                |> Util.cloneWith f
                |> Tuple.mapSecond (writeMemRegister HL state << Carry.value)


applyWordWith : (Word -> Word) -> WordRegister -> State -> State
applyWordWith f register state =
    readWordRegister register state
        |> f
        |> writeWordRegister register state


type Rotation
    = Left
    | Right


rotateA : Rotation -> State -> State
rotateA direction state =
    let
        byte =
            readByteRegister A state

        ( rotation, bit, isBitSet ) =
            case direction of
                Left ->
                    ( Byte.rotateLeft, 0, Byte.msbSet )

                Right ->
                    ( Byte.rotateRight, 7, Byte.lsbSet )
    in
        byte
            |> rotation
            |> Byte.setWith bit (Flag.isSet Flag.Carry state.f)
            |> writeByteRegister A state
            |> resetFlags [ Flag.Zero, Flag.Subtract, Flag.HalfCarry ]
            |> setFlagsWith [ Flag.Carry => isBitSet byte ]
            |> incPC



-- Jumps


shouldJump : FlagCondition -> State -> Bool
shouldJump condition state =
    case condition of
        NoCondition ->
            True

        Set flag ->
            Flag.isSet flag state.f

        NotSet flag ->
            not <| Flag.isSet flag state.f


setJump : Bool -> State -> State
setJump jump state =
    { state | jump = jump }



-- Binary-coded Decimal


type Nibble
    = High
    | Low


bcdCorrect : Nibble -> Bool -> Bool -> Byte -> Carry Byte
bcdCorrect nibble wasCarry wasSub byte =
    let
        ( val, getNibble ) =
            case nibble of
                High ->
                    ( 0x60, Byte.highNibble )

                Low ->
                    ( 0x06, Byte.lowNibble )

        amount =
            if wasSub then
                val * -1
            else
                val
    in
        if wasCarry || getNibble byte > 9 then
            Byte.addc byte <| Byte.fromInt amount
        else
            Carry.withoutOp byte
