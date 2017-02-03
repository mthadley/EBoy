module Z80.Op exposing (..)

{-| Types of Z80 operations.
-}

import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Registers exposing (..)


{-| Jump Targets
-}
type JumpTarget
    = JumpData
    | JumpMemHL


{-| Param for arithmetic operations (`ADD`, `SUB`, etc.).
-}
type Param
    = WithRegister ByteRegister
    | WithMemHL
    | WithData


{-| Target for a CB operation (`Bit`, `Set`, etc.).
-}
type CBParam
    = OnRegister ByteRegister
    | OnMemHL


{-| Flags checked by jump instructions.
-}
type JumpFlag
    = Zero
    | Carry


{-| Represents states when a jump instruction will execute.
-}
type FlagCondition
    = Set JumpFlag
    | NotSet JumpFlag
    | NoCondition


{-| A union type representing all of the supported base set of instructions
of the Z80. There are a few extra "synthetic" instructions added to make
it easier to work with 16-bit (Word) operatoins. These include:

* `ADDW`
* `INCW`
* `LDW`
* `DECW`
* `ADDSP`
-}
type Op
    = INVALID Int
    | NONE
    | NOP
    | LD LB.Target LB.Source
    | LDH LO.Target LO.Source
    | LDW LW.Target LW.Source
    | INC Param
    | INCW WordRegister
    | DEC Param
    | DECW WordRegister
    | RLCA
    | ADD ByteRegister Param
    | ADC ByteRegister Param
    | ADDW WordRegister WordRegister
    | ADDSP
    | RRCA
    | STOP
    | RLA
    | JR FlagCondition
    | RRA
    | LDI LB.Target LB.Source
    | DAA
    | CPL
    | LDD LB.Target LB.Source
    | SCF
    | CCF
    | HALT
    | SUB Param
    | SBC Param
    | AND Param
    | XOR Param
    | OR Param
    | CP Param
    | RET FlagCondition
    | POP WordRegister
    | JP FlagCondition JumpTarget
    | CALL FlagCondition
    | PUSH WordRegister
    | RST Int
    | PREFIX_CB
    | RETI
    | DI
    | EI
    | RLC CBParam
    | RRC CBParam
    | RL CBParam
    | RR CBParam
