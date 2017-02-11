module Z80 exposing (..)

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
            readWordRegister register state
                |> Word.inc
                |> writeWordRegister register state
                |> incPC

        DEC param ->
            applyWith Byte.decc param state
                |> uncurry setIncFlags
                |> setFlag Flag.Subtract
                |> incPC

        DECW register ->
            readWordRegister register state
                |> Word.dec
                |> writeWordRegister register state
                |> incPC

        ADD param ->
            accumulateWith Byte.addc param state
                |> uncurry setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADC param ->
            accumulateCarryWith Byte.addc param state
                |> uncurry setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        _ ->
            state


accumulateWith :
    (Byte -> Byte -> Carry Byte)
    -> ParamData
    -> State
    -> ( Carry Byte, State )
accumulateWith f param state =
    readParamData param state
        |> Tuple.mapFirst (f <| readByteRegister A state)
        |> writeAccumulator


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
            |> writeAccumulator


readParamData : ParamData -> State -> ( Byte, State )
readParamData param state =
    case param of
        WithRegister register ->
            ( readByteRegister register state, state )

        WithMemHL ->
            ( readMemRegister HL state, state )

        WithData ->
            readDataByte state


writeAccumulator : ( Carry Byte, State ) -> ( Carry Byte, State )
writeAccumulator ( result, state ) =
    ( result
    , writeByteRegister A state <| Carry.value result
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
