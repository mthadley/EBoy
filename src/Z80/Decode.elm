module Z80.Decode exposing (decode, decodeCB, isCB)

import Bitwise
import Byte exposing (Byte)
import Z80.Cycles exposing (Cycles(..))
import Z80.Flag exposing (..)
import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Op exposing (..)
import Z80.Registers exposing (..)


{-| Decodes an instruction, returning a tuple of the `Op` and
how many `Cycle`'s' it will take.
-}
decode : Byte -> ( Op, Cycles )
decode b =
    case Byte.toInt b of
        0x00 ->
            det NOP 4

        0x01 ->
            det (LDW (LW.IntoRegister BC) LW.FromData) 12

        0x02 ->
            det (LD (LB.IntoMem BC) (LB.FromRegister A)) 8

        0x03 ->
            det (INCW BC) 8

        0x04 ->
            det (INC (OnRegister B)) 4

        0x05 ->
            det (DEC (OnRegister B)) 4

        0x06 ->
            det (LD (LB.IntoRegister B) LB.FromData) 8

        0x07 ->
            det (RLC (OnRegister A)) 4

        0x08 ->
            det (LDW LW.IntoMemData (LW.FromRegister SP)) 20

        0x09 ->
            det (ADDW BC) 8

        0x0A ->
            det (LD (LB.IntoRegister A) (LB.FromMem BC)) 8

        0x0B ->
            det (DECW BC) 8

        0x0C ->
            det (INC (OnRegister C)) 4

        0x0D ->
            det (DEC (OnRegister C)) 4

        0x0E ->
            det (LD (LB.IntoRegister C) LB.FromData) 8

        0x0F ->
            det RRCA 4

        0x10 ->
            det STOP 4

        0x11 ->
            det (LDW (LW.IntoRegister DE) LW.FromData) 12

        0x12 ->
            det (LD (LB.IntoMem DE) (LB.FromRegister A)) 8

        0x13 ->
            det (INCW DE) 8

        0x14 ->
            det (INC (OnRegister D)) 4

        0x15 ->
            det (DEC (OnRegister D)) 4

        0x16 ->
            det (LD (LB.IntoRegister D) LB.FromData) 8

        0x17 ->
            det RLA 4

        0x18 ->
            det (JR NoCondition) 12

        0x19 ->
            det (ADDW DE) 8

        0x1A ->
            det (LD (LB.IntoRegister A) (LB.FromMem DE)) 8

        0x1B ->
            det (DECW DE) 8

        0x1C ->
            det (INC (OnRegister E)) 4

        0x1D ->
            det (DEC (OnRegister E)) 4

        0x1E ->
            det (LD (LB.IntoRegister E) LB.FromData) 8

        0x1F ->
            det RRA 4

        0x20 ->
            br (JR (NotSet Zero)) ( 12, 8 )

        0x21 ->
            det (LDW (LW.IntoRegister HL) LW.FromData) 12

        0x22 ->
            det (LDI (LB.IntoMem HL) (LB.FromRegister A)) 8

        0x23 ->
            det (INCW HL) 8

        0x24 ->
            det (INC (OnRegister H)) 4

        0x25 ->
            det (DEC (OnRegister H)) 4

        0x26 ->
            det (LD (LB.IntoRegister H) LB.FromData) 8

        0x27 ->
            det DAA 4

        0x28 ->
            br (JR (Set Zero)) ( 12, 8 )

        0x29 ->
            det (ADDW HL) 8

        0x2A ->
            det (LDI (LB.IntoRegister A) (LB.FromMem HL)) 8

        0x2B ->
            det (DECW HL) 8

        0x2C ->
            det (INC (OnRegister L)) 4

        0x2D ->
            det (DEC (OnRegister L)) 4

        0x2E ->
            det (LD (LB.IntoRegister L) LB.FromData) 8

        0x2F ->
            det CPL 4

        0x30 ->
            br (JR (NotSet Carry)) ( 12, 8 )

        0x31 ->
            det (LDW (LW.IntoRegister SP) LW.FromData) 12

        0x32 ->
            det (LDD (LB.IntoMem HL) (LB.FromRegister A)) 8

        0x33 ->
            det (INCW SP) 8

        0x34 ->
            det (INC OnMemHL) 12

        0x35 ->
            det (DEC OnMemHL) 12

        0x36 ->
            det (LD (LB.IntoMem HL) LB.FromData) 12

        0x37 ->
            det SCF 4

        0x38 ->
            br (JR (Set Carry)) ( 12, 8 )

        0x39 ->
            det (ADDW SP) 8

        0x3A ->
            det (LDD (LB.IntoRegister A) (LB.FromMem HL)) 8

        0x3B ->
            det (DECW SP) 8

        0x3C ->
            det (INC (OnRegister A)) 4

        0x3D ->
            det (DEC (OnRegister A)) 4

        0x3E ->
            det (LD (LB.IntoRegister A) LB.FromData) 8

        0x3F ->
            det CCF 4

        0x40 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister B)) 4

        0x41 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister C)) 4

        0x42 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister D)) 4

        0x43 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister E)) 4

        0x44 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister H)) 4

        0x45 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister L)) 4

        0x46 ->
            det (LD (LB.IntoRegister B) (LB.FromMem HL)) 8

        0x47 ->
            det (LD (LB.IntoRegister B) (LB.FromRegister A)) 4

        0x48 ->
            det (LD (LB.IntoRegister C) (LB.FromRegister B)) 4

        0x49 ->
            det (LD (LB.IntoRegister C) (LB.FromRegister C)) 4

        0x4A ->
            det (LD (LB.IntoRegister C) (LB.FromRegister D)) 4

        0x4B ->
            det (LD (LB.IntoRegister C) (LB.FromRegister E)) 4

        0x4C ->
            det (LD (LB.IntoRegister C) (LB.FromRegister H)) 4

        0x4D ->
            det (LD (LB.IntoRegister C) (LB.FromRegister L)) 4

        0x4E ->
            det (LD (LB.IntoRegister C) (LB.FromMem HL)) 8

        0x4F ->
            det (LD (LB.IntoRegister C) (LB.FromRegister A)) 4

        0x50 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister B)) 4

        0x51 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister C)) 4

        0x52 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister D)) 4

        0x53 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister E)) 4

        0x54 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister H)) 4

        0x55 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister L)) 4

        0x56 ->
            det (LD (LB.IntoRegister D) (LB.FromMem HL)) 8

        0x57 ->
            det (LD (LB.IntoRegister D) (LB.FromRegister A)) 4

        0x58 ->
            det (LD (LB.IntoRegister E) (LB.FromRegister B)) 4

        0x59 ->
            det (LD (LB.IntoRegister E) (LB.FromRegister C)) 4

        0x5A ->
            det (LD (LB.IntoRegister E) (LB.FromRegister D)) 4

        0x5B ->
            det (LD (LB.IntoRegister E) (LB.FromRegister E)) 4

        0x5C ->
            det (LD (LB.IntoRegister E) (LB.FromRegister H)) 4

        0x5D ->
            det (LD (LB.IntoRegister E) (LB.FromRegister L)) 4

        0x5E ->
            det (LD (LB.IntoRegister E) (LB.FromMem HL)) 8

        0x5F ->
            det (LD (LB.IntoRegister E) (LB.FromRegister A)) 4

        0x60 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister B)) 4

        0x61 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister C)) 4

        0x62 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister D)) 4

        0x63 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister E)) 4

        0x64 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister H)) 4

        0x65 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister L)) 4

        0x66 ->
            det (LD (LB.IntoRegister H) (LB.FromMem HL)) 8

        0x67 ->
            det (LD (LB.IntoRegister H) (LB.FromRegister A)) 4

        0x68 ->
            det (LD (LB.IntoRegister L) (LB.FromRegister B)) 4

        0x69 ->
            det (LD (LB.IntoRegister L) (LB.FromRegister C)) 4

        0x6A ->
            det (LD (LB.IntoRegister L) (LB.FromRegister D)) 4

        0x6B ->
            det (LD (LB.IntoRegister L) (LB.FromRegister E)) 4

        0x6C ->
            det (LD (LB.IntoRegister L) (LB.FromRegister H)) 4

        0x6D ->
            det (LD (LB.IntoRegister L) (LB.FromRegister L)) 4

        0x6E ->
            det (LD (LB.IntoRegister L) (LB.FromMem HL)) 8

        0x6F ->
            det (LD (LB.IntoRegister L) (LB.FromRegister A)) 4

        0x70 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister B)) 8

        0x71 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister C)) 8

        0x72 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister D)) 8

        0x73 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister E)) 8

        0x74 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister H)) 8

        0x75 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister L)) 8

        0x76 ->
            det HALT 4

        0x77 ->
            det (LD (LB.IntoMem HL) (LB.FromRegister A)) 8

        0x78 ->
            det (LD (LB.IntoRegister A) (LB.FromRegister B)) 4

        0x79 ->
            det (LD (LB.IntoRegister A) (LB.FromRegister C)) 4

        0x7A ->
            det (LD (LB.IntoRegister A) (LB.FromRegister D)) 4

        0x7B ->
            det (LD (LB.IntoRegister A) (LB.FromRegister E)) 4

        0x7C ->
            det (LD (LB.IntoRegister A) (LB.FromRegister H)) 4

        0x7D ->
            det (LD (LB.IntoRegister A) (LB.FromRegister L)) 4

        0x7E ->
            det (LD (LB.IntoRegister A) (LB.FromMem HL)) 8

        0x7F ->
            det (LD (LB.IntoRegister A) (LB.FromRegister A)) 8

        0x80 ->
            det (ADD (WithRegister B)) 4

        0x81 ->
            det (ADD (WithRegister C)) 4

        0x82 ->
            det (ADD (WithRegister D)) 4

        0x83 ->
            det (ADD (WithRegister E)) 4

        0x84 ->
            det (ADD (WithRegister H)) 4

        0x85 ->
            det (ADD (WithRegister L)) 4

        0x86 ->
            det (ADD WithMemHL) 8

        0x87 ->
            det (ADD (WithRegister A)) 4

        0x88 ->
            det (ADC (WithRegister B)) 4

        0x89 ->
            det (ADC (WithRegister C)) 4

        0x8A ->
            det (ADC (WithRegister D)) 4

        0x8B ->
            det (ADC (WithRegister E)) 4

        0x8C ->
            det (ADC (WithRegister H)) 4

        0x8D ->
            det (ADC (WithRegister L)) 4

        0x8E ->
            det (ADC WithMemHL) 8

        0x8F ->
            det (ADC (WithRegister A)) 4

        0x90 ->
            det (SUB (WithRegister B)) 4

        0x91 ->
            det (SUB (WithRegister C)) 4

        0x92 ->
            det (SUB (WithRegister D)) 4

        0x93 ->
            det (SUB (WithRegister E)) 4

        0x94 ->
            det (SUB (WithRegister H)) 4

        0x95 ->
            det (SUB (WithRegister L)) 4

        0x96 ->
            det (SUB WithMemHL) 8

        0x97 ->
            det (SUB (WithRegister A)) 4

        0x98 ->
            det (SBC (WithRegister B)) 4

        0x99 ->
            det (SBC (WithRegister C)) 4

        0x9A ->
            det (SBC (WithRegister D)) 4

        0x9B ->
            det (SBC (WithRegister E)) 4

        0x9C ->
            det (SBC (WithRegister H)) 4

        0x9D ->
            det (SBC (WithRegister L)) 4

        0x9E ->
            det (SBC WithMemHL) 8

        0x9F ->
            det (SBC (WithRegister A)) 4

        0xA0 ->
            det (AND (WithRegister B)) 4

        0xA1 ->
            det (AND (WithRegister C)) 4

        0xA2 ->
            det (AND (WithRegister D)) 4

        0xA3 ->
            det (AND (WithRegister E)) 4

        0xA4 ->
            det (AND (WithRegister H)) 4

        0xA5 ->
            det (AND (WithRegister L)) 4

        0xA6 ->
            det (AND WithMemHL) 8

        0xA7 ->
            det (AND (WithRegister A)) 4

        0xA8 ->
            det (XOR (WithRegister B)) 4

        0xA9 ->
            det (XOR (WithRegister C)) 4

        0xAA ->
            det (XOR (WithRegister D)) 4

        0xAB ->
            det (XOR (WithRegister E)) 4

        0xAC ->
            det (XOR (WithRegister H)) 4

        0xAD ->
            det (XOR (WithRegister L)) 4

        0xAE ->
            det (XOR WithMemHL) 8

        0xAF ->
            det (XOR (WithRegister A)) 4

        0xB0 ->
            det (OR (WithRegister B)) 4

        0xB1 ->
            det (OR (WithRegister C)) 4

        0xB2 ->
            det (OR (WithRegister D)) 4

        0xB3 ->
            det (OR (WithRegister E)) 4

        0xB4 ->
            det (OR (WithRegister H)) 4

        0xB5 ->
            det (OR (WithRegister L)) 4

        0xB6 ->
            det (OR WithMemHL) 8

        0xB7 ->
            det (OR (WithRegister A)) 4

        0xB8 ->
            det (CP (WithRegister B)) 4

        0xB9 ->
            det (CP (WithRegister C)) 4

        0xBA ->
            det (CP (WithRegister D)) 4

        0xBB ->
            det (CP (WithRegister E)) 4

        0xBC ->
            det (CP (WithRegister H)) 4

        0xBD ->
            det (CP (WithRegister L)) 4

        0xBE ->
            det (CP WithMemHL) 8

        0xBF ->
            det (CP (WithRegister A)) 4

        0xC0 ->
            br (RET (NotSet Zero)) ( 20, 8 )

        0xC1 ->
            det (POP BC) 12

        0xC2 ->
            br (JP (NotSet Zero) JumpData) ( 16, 12 )

        0xC3 ->
            br (JP NoCondition JumpData) ( 16, 12 )

        0xC4 ->
            br (CALL (NotSet Zero)) ( 24, 12 )

        0xC5 ->
            det (PUSH BC) 16

        0xC6 ->
            det (ADD WithData) 8

        0xC7 ->
            det (RST 0x00) 16

        0xC8 ->
            br (RET (Set Zero)) ( 20, 8 )

        0xC9 ->
            det (RET NoCondition) 16

        0xCA ->
            br (JP (Set Zero) JumpData) ( 16, 12 )

        0xCB ->
            det PREFIX_CB 4

        0xCC ->
            br (CALL (Set Zero)) ( 24, 12 )

        0xCD ->
            det (CALL NoCondition) 24

        0xCE ->
            det (ADC WithData) 8

        0xCF ->
            det (RST 0x08) 16

        0xD0 ->
            br (RET (NotSet Carry)) ( 20, 8 )

        0xD1 ->
            det (POP DE) 12

        0xD2 ->
            br (JP (NotSet Carry) JumpData) ( 16, 12 )

        0xD3 ->
            none

        0xD4 ->
            br (CALL (NotSet Carry)) ( 24, 12 )

        0xD5 ->
            det (PUSH DE) 16

        0xD6 ->
            det (SUB WithData) 8

        0xD7 ->
            det (RST 0x10) 16

        0xD8 ->
            br (RET (Set Carry)) ( 20, 8 )

        0xD9 ->
            det RETI 16

        0xDA ->
            br (JP (Set Carry) JumpData) ( 16, 12 )

        0xDB ->
            none

        0xDC ->
            br (CALL (Set Carry)) ( 24, 12 )

        0xDD ->
            none

        0xDE ->
            det (SBC (WithRegister A)) 8

        0xDF ->
            det (RST 0x18) 16

        0xE0 ->
            det (LDH LO.IntoMemDataOffset LO.FromRegisterA) 12

        0xE1 ->
            det (POP HL) 12

        0xE2 ->
            det (LDH LO.IntoMemCOffset LO.FromRegisterA) 8

        0xE3 ->
            none

        0xE4 ->
            none

        0xE5 ->
            det (PUSH HL) 16

        0xE6 ->
            det (AND WithData) 8

        0xE7 ->
            det (RST 0x20) 16

        0xE8 ->
            det ADDSP 16

        0xE9 ->
            det (JP NoCondition JumpMemHL) 4

        0xEA ->
            det (LD LB.IntoMemData (LB.FromRegister A)) 16

        0xEB ->
            none

        0xEC ->
            none

        0xED ->
            none

        0xEE ->
            det (XOR WithData) 8

        0xEF ->
            det (RST 0x28) 16

        0xF0 ->
            det (LDH LO.IntoRegisterA LO.FromMemDataOffset) 12

        0xF1 ->
            det (POP AF) 12

        0xF2 ->
            det (LDH LO.IntoRegisterA LO.FromMemCOffset) 8

        0xF3 ->
            det DI 4

        0xF4 ->
            none

        0xF5 ->
            det (PUSH AF) 16

        0xF6 ->
            det (OR WithData) 8

        0xF7 ->
            det (RST 0x30) 16

        0xF8 ->
            det (LDW (LW.IntoRegister HL) LW.FromSPByteData) 12

        0xF9 ->
            det (LDW (LW.IntoRegister SP) (LW.FromRegister HL)) 8

        0xFA ->
            det (LD LB.IntoMemData (LB.FromRegister A)) 16

        0xFB ->
            det EI 4

        0xFC ->
            none

        0xFD ->
            none

        0xFE ->
            det (CP WithData) 8

        0xFF ->
            det (RST 0x38) 16

        byte ->
            det (INVALID byte) 0


{-| Decodes a CB instruction, returning a tuple of the `Op` and
how many `Cycle`'s' it will take. This is implemented by separating
the 1 byte instruction space into 32 groups, each group containing 8
instructions that follow the order defined below.
-}
decodeCB : Byte -> ( Op, Cycles )
decodeCB byte =
    let
        b =
            Byte.toInt byte
    in
    decodeCBWith (decodeCBOp b) b


decodeCBOp : Int -> (Param -> Op)
decodeCBOp code =
    case code // 8 of
        0 ->
            RLC

        1 ->
            RRC

        2 ->
            RL

        3 ->
            RR

        4 ->
            SLA

        5 ->
            SRA

        6 ->
            SWAP

        7 ->
            SRL

        8 ->
            BIT 0

        9 ->
            BIT 1

        10 ->
            BIT 2

        11 ->
            BIT 3

        12 ->
            BIT 4

        13 ->
            BIT 5

        14 ->
            BIT 6

        15 ->
            BIT 7

        16 ->
            RES 0

        17 ->
            RES 1

        18 ->
            RES 2

        19 ->
            RES 3

        20 ->
            RES 4

        21 ->
            RES 5

        22 ->
            RES 6

        23 ->
            RES 7

        24 ->
            SET 0

        25 ->
            SET 1

        26 ->
            SET 2

        27 ->
            SET 3

        28 ->
            SET 4

        29 ->
            SET 5

        30 ->
            SET 6

        31 ->
            SET 7

        b ->
            always <| INVALID b


decodeCBWith : (Param -> Op) -> Int -> ( Op, Cycles )
decodeCBWith op code =
    let
        ( param, cycles ) =
            case modBy 8 (Bitwise.and code 0x0F) of
                0 ->
                    ( OnRegister B, 8 )

                1 ->
                    ( OnRegister C, 8 )

                2 ->
                    ( OnRegister D, 8 )

                3 ->
                    ( OnRegister E, 8 )

                4 ->
                    ( OnRegister H, 8 )

                5 ->
                    ( OnRegister L, 8 )

                6 ->
                    ( OnMemHL, 16 )

                _ ->
                    ( OnRegister A, 8 )
    in
    det (op param) cycles


{-| Returns `True` if the operation is the `PREFIX_CB`.
-}
isCB : Op -> Bool
isCB op =
    case op of
        PREFIX_CB ->
            True

        _ ->
            False


none : ( Op, Cycles )
none =
    det NONE 0


{-| Returns a tuple containing the operation and number of
cycles it will take.
-}
det : Op -> Int -> ( Op, Cycles )
det op cycles =
    ( op, Always cycles )


{-| det (Same as `) (` but for branching instructions. Also takes a)
tuple instead of just an `Int`, where the first is if the
branch is taken, and the second if it is not.
-}
br : Op -> ( Int, Int ) -> ( Op, Cycles )
br op ( taken, notTaken ) =
    ( op, Branching taken notTaken )
