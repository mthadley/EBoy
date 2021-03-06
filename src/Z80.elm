module Z80 exposing (next)

import Byte exposing (Byte)
import Carry exposing (Carry)
import Util
import Word exposing (Word)
import Z80.Cycles exposing (Cycles)
import Z80.Decode as Decode
import Z80.Flag as Flag exposing (Flag)
import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.MMU as MMU
import Z80.Mode as Mode
import Z80.Op exposing (..)
import Z80.Registers exposing (..)
import Z80.State as State exposing (..)


next : State -> State
next =
    execute << decode << fetch


fetch : State -> ( Byte, State )
fetch state =
    ( MMU.readByte state.pc state.mmu
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
            Debug.todo <| String.fromInt x

        NONE ->
            incPC state

        NOP ->
            incPC state

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
                |> (\( a, b ) -> setIncFlags a b)
                |> resetFlag Flag.Subtract
                |> incPC

        INCW register ->
            incPC <| applyWordWith Word.inc register state

        DEC param ->
            applyWith Byte.decc param state
                |> (\( a, b ) -> setIncFlags a b)
                |> setFlag Flag.Subtract
                |> incPC

        DECW register ->
            incPC <| applyWordWith Word.dec register state

        ADD param ->
            accCarryWith Byte.addc param state
                |> setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADC param ->
            accPlusCarryWith Byte.addc param state
                |> setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        ADDW register ->
            readWordRegister register state
                |> Word.addc (readWordRegister HL state)
                |> Util.clone
                |> Tuple.mapSecond (writeWordRegister register state << Carry.value)
                |> (\( a, b ) -> setCarryFlags a b)
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
                |> (\( a, b ) -> setCarryFlags a b)
                |> resetFlags [ Flag.Zero, Flag.Subtract ]
                |> incPC

        RRCA ->
            rotate (OnRegister A) Right False state
                |> resetFlags
                    [ Flag.Zero
                    , Flag.Subtract
                    , Flag.HalfCarry
                    ]
                |> incPC

        STOP ->
            incPC <| { state | mode = Mode.Stopped }

        RLA ->
            incPC <| rotate (OnRegister A) Left True state

        JR condition ->
            if shouldJump condition state then
                readDataByte state
                    |> (\( a, b ) -> addPCSigned a b)
                    |> setJump
                    |> incPC

            else
                readDataByte state
                    |> Tuple.second
                    |> incPC

        RRA ->
            rotate (OnRegister A) Right True state
                |> resetFlags
                    [ Flag.Zero
                    , Flag.Subtract
                    , Flag.HalfCarry
                    ]
                |> incPC

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
                |> setAccFlags
                |> resetFlag Flag.HalfCarry
                |> incPC

        CPL ->
            readByteRegister A state
                |> Byte.complement
                |> writeByteRegister A state
                |> setFlags [ Flag.HalfCarry, Flag.Subtract ]
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
                |> setFlagsWith [ ( Flag.Carry, not <| Flag.isSet Flag.Carry state.f ) ]
                |> resetFlags [ Flag.HalfCarry, Flag.Subtract ]
                |> incPC

        HALT ->
            incPC <| { state | mode = Mode.Halted }

        SUB param ->
            accCarryWith Byte.subc param state
                |> setAccFlags
                |> setFlag Flag.Subtract
                |> incPC

        SBC param ->
            accPlusCarryWith Byte.subc param state
                |> setAccFlags
                |> setFlag Flag.Subtract
                |> incPC

        AND param ->
            accByteWith Byte.and param state
                |> (\( a, b ) -> setZeroFlag a b)
                |> resetFlags [ Flag.Subtract, Flag.Carry ]
                |> setFlag Flag.HalfCarry
                |> incPC

        OR param ->
            accByteWith Byte.or param state
                |> (\( a, b ) -> setZeroFlag a b)
                |> resetFlags [ Flag.Subtract, Flag.Carry, Flag.HalfCarry ]
                |> incPC

        XOR param ->
            accByteWith Byte.xor param state
                |> (\( a, b ) -> setZeroFlag a b)
                |> resetFlags [ Flag.Subtract, Flag.Carry, Flag.HalfCarry ]
                |> incPC

        CP param ->
            readParamData param state
                |> Tuple.mapFirst (Byte.subc <| readByteRegister A state)
                |> setAccFlags
                |> setFlag Flag.Subtract
                |> incPC

        RET condition ->
            if shouldJump condition state then
                setJump <| popSP PC state

            else
                incPC <| state

        POP register ->
            incPC <| popSP register state

        JP condition target ->
            if shouldJump condition state then
                readJumpTarget target state
                    |> writeWordRegister PC state
                    |> setJump

            else
                incPC <| state

        CALL condition ->
            if shouldJump condition state then
                readWordRegister PC state
                    |> Word.addInt 3
                    |> pushSPWord state
                    |> readDataWord
                    |> (\( w, newState ) -> writeWordRegister PC newState w)

            else
                incPC <| state

        PUSH register ->
            incPC <| pushSP register state

        RST offset ->
            writeWordRegister PC (pushSP PC state) (Word.fromInt offset)

        PREFIX_CB ->
            incPC state

        RETI ->
            enableInterrupts True <| popSP PC state

        DI ->
            incPC <| enableInterrupts False state

        EI ->
            incPC <| enableInterrupts True state

        RLC param ->
            incPC <| rotate param Left False state

        RRC param ->
            incPC <| rotate param Right False state

        RL param ->
            incPC <| rotate param Left True state

        RR param ->
            incPC <| rotate param Right True state

        SLA param ->
            applyWith Byte.shiftLeft param state
                |> setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        SRA param ->
            applyWith Byte.shiftRight param state
                |> setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        SWAP param ->
            let
                result =
                    Byte.swap <| readParam param state
            in
            writeParam param state result
                |> resetFlags [ Flag.Subtract, Flag.Carry, Flag.HalfCarry ]
                |> setFlagsWith [ ( Flag.Zero, Byte.isZero result ) ]
                |> incPC

        SRL param ->
            applyWith Byte.shiftRightZf param state
                |> setAccFlags
                |> resetFlag Flag.Subtract
                |> incPC

        BIT bit param ->
            let
                isSet =
                    Byte.getBit bit <| readParam param state
            in
            state
                |> setFlagsWith [ ( Flag.Zero, not isSet ) ]
                |> setFlag Flag.HalfCarry
                |> resetFlag Flag.Subtract
                |> incPC

        RES bit param ->
            readParam param state
                |> Byte.reset bit
                |> writeParam param state
                |> incPC

        SET bit param ->
            readParam param state
                |> Byte.set bit
                |> writeParam param state
                |> incPC


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


accPlusCarryWith :
    (Byte -> Byte -> Carry Byte)
    -> ParamData
    -> State
    -> ( Carry Byte, State )
accPlusCarryWith f param state =
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
    readParam param state
        |> Util.cloneWith f
        |> Tuple.mapSecond (writeParam param state << Carry.value)


applyWordWith : (Word -> Word) -> WordRegister -> State -> State
applyWordWith f register state =
    readWordRegister register state
        |> f
        |> writeWordRegister register state


type Rotation
    = Left
    | Right


rotate : Param -> Rotation -> Bool -> State -> State
rotate param direction throughCarry state =
    let
        byte =
            readParam param state

        ( rotation, bit, isBitSet ) =
            case direction of
                Left ->
                    ( Byte.rotateLeft, 0, Byte.msbSet )

                Right ->
                    ( Byte.rotateRight, 7, Byte.lsbSet )

        carryIn =
            if throughCarry then
                Byte.setWith bit (Flag.isSet Flag.Carry state.f)

            else
                identity

        result =
            carryIn <| rotation <| byte
    in
    result
        |> writeParam param state
        |> resetFlags [ Flag.Subtract, Flag.HalfCarry ]
        |> setFlagsWith
            [ ( Flag.Carry, isBitSet byte )
            , ( Flag.Zero, Byte.isZero result )
            ]


readParam : Param -> State -> Byte
readParam param state =
    case param of
        OnRegister register ->
            readByteRegister register state

        OnMemHL ->
            readMemRegister HL state


writeParam : Param -> State -> Byte -> State
writeParam param state byte =
    case param of
        OnRegister register ->
            writeByteRegister register state byte

        OnMemHL ->
            writeMemRegister HL state byte



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


setJump : State -> State
setJump state =
    { state | jump = True }


readJumpTarget : JumpTarget -> State -> Word
readJumpTarget target state =
    case target of
        JumpData ->
            Tuple.first <| readDataWord state

        JumpMemHL ->
            readMemWordRegister HL state



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
