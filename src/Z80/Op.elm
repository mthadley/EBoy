module Z80.Op exposing (..)

{-| Types of Z80 operations.
-}

import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Registers exposing (..)
import Z80.Flag exposing (Flag)


{-| Jump Targets
-}
type JumpTarget
    = JumpData
    | JumpMemHL


{-| Param for arithmetic operations (`ADD`, `SUB`, etc.).
-}
type ParamData
    = WithRegister ByteRegister
    | WithMemHL
    | WithData


{-| Target for a unary operation (`Bit`, `Set`, etc.).
-}
type Param
    = OnRegister ByteRegister
    | OnMemHL


{-| Represents states when a jump instruction will execute.
-}
type FlagCondition
    = Set Flag
    | NotSet Flag
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
    | ADD ParamData
    | ADC ParamData
    | ADDW WordRegister
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
    | SUB ParamData
    | SBC ParamData
    | AND ParamData
    | OR ParamData
    | XOR ParamData
    | CP ParamData
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
    | RLC Param
    | RRC Param
    | RL Param
    | RR Param
    | SLA Param
    | SRA Param
    | SWAP Param
    | SRL Param
    | BIT Int Param
    | RES Int Param
    | SET Int Param
