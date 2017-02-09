module Z80 exposing (..)

import Carry exposing (Carry)
import Basics.Extra exposing ((=>))
import Byte exposing (Byte)
import Memory exposing (Memory)
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
            let
                ( result, newState ) =
                    applyParamWith Byte.incc param state
            in
                newState
                    |> setFlagsWith
                        [ Flag.HalfCarry => Carry.checkHalf result
                        , Flag.Subtract => False
                        , Flag.Zero => (Byte.isZero <| Carry.value <| result)
                        ]
                    |> incPC

        _ ->
            state


applyParamWith : (Byte -> Carry Byte) -> Param -> State -> ( Carry Byte, State )
applyParamWith f param state =
    case param of
        OnRegister register ->
            let
                result =
                    f <| readByteRegister register state
            in
                ( result
                , writeByteRegister register state <| Carry.value result
                )

        OnMemHL ->
            let
                result =
                    f <| readMemRegister HL state
            in
                ( result
                , writeMemRegister HL state <| Carry.value result
                )
