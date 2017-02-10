module Z80.Decode exposing (decode, decodeCB, isCB)

import Bitwise
import Byte exposing (Byte)
import Z80.LB as LB
import Z80.LO as LO
import Z80.LW as LW
import Z80.Op exposing (..)
import Z80.Registers exposing (..)
import Z80.Cycles exposing (Cycles(..))


{-| Decodes an instruction, returning a tuple of the `Op` and
how many `Cycle`'s' it will take.
-}
decode : Byte -> ( Op, Cycles )
decode b =
    case Byte.toInt b of
        0x00 ->
            NOP @ 4

        0x01 ->
            LDW (LW.IntoRegister BC) LW.FromData @ 12

        0x02 ->
            LD (LB.IntoMem BC) (LB.FromRegister A) @ 8

        0x03 ->
            INCW BC @ 8

        0x04 ->
            INC (OnRegister B) @ 4

        0x05 ->
            DEC (OnRegister B) @ 4

        0x06 ->
            LD (LB.IntoRegister B) LB.FromData @ 8

        0x07 ->
            RLC (OnRegister A) @ 4

        0x08 ->
            LDW LW.IntoMemData (LW.FromRegister SP) @ 20

        0x09 ->
            ADDW HL BC @ 8

        0x0A ->
            LD (LB.IntoRegister A) (LB.FromMem BC) @ 8

        0x0B ->
            DECW BC @ 8

        0x0C ->
            INC (OnRegister C) @ 4

        0x0D ->
            DEC (OnRegister C) @ 4

        0x0E ->
            LD (LB.IntoRegister C) LB.FromData @ 8

        0x0F ->
            RRCA @ 4

        0x10 ->
            STOP @ 4

        0x11 ->
            LDW (LW.IntoRegister DE) LW.FromData @ 12

        0x12 ->
            LD (LB.IntoMem DE) (LB.FromRegister A) @ 8

        0x13 ->
            INCW DE @ 8

        0x14 ->
            INC (OnRegister D) @ 4

        0x15 ->
            DEC (OnRegister D) @ 4

        0x16 ->
            LD (LB.IntoRegister D) LB.FromData @ 8

        0x17 ->
            RLA @ 4

        0x18 ->
            JR NoCondition @ 12

        0x19 ->
            ADDW HL DE @ 8

        0x1A ->
            LD (LB.IntoRegister A) (LB.FromMem DE) @ 8

        0x1B ->
            DECW DE @ 8

        0x1C ->
            INC (OnRegister E) @ 4

        0x1D ->
            DEC (OnRegister E) @ 4

        0x1E ->
            LD (LB.IntoRegister E) LB.FromData @ 8

        0x1F ->
            RRA @ 4

        0x20 ->
            JR (NotSet Zero) / ( 12, 8 )

        0x21 ->
            LDW (LW.IntoRegister HL) LW.FromData @ 12

        0x22 ->
            LDI (LB.IntoMem HL) (LB.FromRegister A) @ 8

        0x23 ->
            INCW HL @ 8

        0x24 ->
            INC (OnRegister H) @ 4

        0x25 ->
            DEC (OnRegister H) @ 4

        0x26 ->
            LD (LB.IntoRegister H) LB.FromData @ 8

        0x27 ->
            DAA @ 4

        0x28 ->
            JR (Set Zero) / ( 12, 8 )

        0x29 ->
            ADDW HL HL @ 8

        0x2A ->
            LDI (LB.IntoRegister A) (LB.FromMem HL) @ 8

        0x2B ->
            DECW HL @ 8

        0x2C ->
            INC (OnRegister L) @ 4

        0x2D ->
            DEC (OnRegister L) @ 4

        0x2E ->
            LD (LB.IntoRegister L) LB.FromData @ 8

        0x2F ->
            CPL @ 4

        0x30 ->
            JR (NotSet Carry) / ( 12, 8 )

        0x31 ->
            LDW (LW.IntoRegister SP) LW.FromData @ 12

        0x32 ->
            LDD (LB.IntoMem HL) (LB.FromRegister A) @ 8

        0x33 ->
            INCW SP @ 8

        0x34 ->
            INC OnMemHL @ 12

        0x35 ->
            DEC OnMemHL @ 12

        0x36 ->
            LD (LB.IntoMem HL) LB.FromData @ 12

        0x37 ->
            SCF @ 4

        0x38 ->
            JR (Set Carry) / ( 12, 8 )

        0x39 ->
            ADDW HL SP @ 8

        0x3A ->
            LDD (LB.IntoRegister A) (LB.FromMem HL) @ 8

        0x3B ->
            DECW SP @ 8

        0x3C ->
            INC (OnRegister A) @ 4

        0x3D ->
            DEC (OnRegister A) @ 4

        0x3E ->
            LD (LB.IntoRegister A) LB.FromData @ 8

        0x3F ->
            CCF @ 4

        0x40 ->
            LD (LB.IntoRegister B) (LB.FromRegister B) @ 4

        0x41 ->
            LD (LB.IntoRegister B) (LB.FromRegister C) @ 4

        0x42 ->
            LD (LB.IntoRegister B) (LB.FromRegister D) @ 4

        0x43 ->
            LD (LB.IntoRegister B) (LB.FromRegister E) @ 4

        0x44 ->
            LD (LB.IntoRegister B) (LB.FromRegister H) @ 4

        0x45 ->
            LD (LB.IntoRegister B) (LB.FromRegister L) @ 4

        0x46 ->
            LD (LB.IntoRegister B) (LB.FromMem HL) @ 8

        0x47 ->
            LD (LB.IntoRegister B) (LB.FromRegister A) @ 4

        0x48 ->
            LD (LB.IntoRegister C) (LB.FromRegister B) @ 4

        0x49 ->
            LD (LB.IntoRegister C) (LB.FromRegister C) @ 4

        0x4A ->
            LD (LB.IntoRegister C) (LB.FromRegister D) @ 4

        0x4B ->
            LD (LB.IntoRegister C) (LB.FromRegister E) @ 4

        0x4C ->
            LD (LB.IntoRegister C) (LB.FromRegister H) @ 4

        0x4D ->
            LD (LB.IntoRegister C) (LB.FromRegister L) @ 4

        0x4E ->
            LD (LB.IntoRegister C) (LB.FromMem HL) @ 8

        0x4F ->
            LD (LB.IntoRegister C) (LB.FromRegister A) @ 4

        0x50 ->
            LD (LB.IntoRegister D) (LB.FromRegister B) @ 4

        0x51 ->
            LD (LB.IntoRegister D) (LB.FromRegister C) @ 4

        0x52 ->
            LD (LB.IntoRegister D) (LB.FromRegister D) @ 4

        0x53 ->
            LD (LB.IntoRegister D) (LB.FromRegister E) @ 4

        0x54 ->
            LD (LB.IntoRegister D) (LB.FromRegister H) @ 4

        0x55 ->
            LD (LB.IntoRegister D) (LB.FromRegister L) @ 4

        0x56 ->
            LD (LB.IntoRegister D) (LB.FromMem HL) @ 8

        0x57 ->
            LD (LB.IntoRegister D) (LB.FromRegister A) @ 4

        0x58 ->
            LD (LB.IntoRegister E) (LB.FromRegister B) @ 4

        0x59 ->
            LD (LB.IntoRegister E) (LB.FromRegister C) @ 4

        0x5A ->
            LD (LB.IntoRegister E) (LB.FromRegister D) @ 4

        0x5B ->
            LD (LB.IntoRegister E) (LB.FromRegister E) @ 4

        0x5C ->
            LD (LB.IntoRegister E) (LB.FromRegister H) @ 4

        0x5D ->
            LD (LB.IntoRegister E) (LB.FromRegister L) @ 4

        0x5E ->
            LD (LB.IntoRegister E) (LB.FromMem HL) @ 8

        0x5F ->
            LD (LB.IntoRegister E) (LB.FromRegister A) @ 4

        0x60 ->
            LD (LB.IntoRegister H) (LB.FromRegister B) @ 4

        0x61 ->
            LD (LB.IntoRegister H) (LB.FromRegister C) @ 4

        0x62 ->
            LD (LB.IntoRegister H) (LB.FromRegister D) @ 4

        0x63 ->
            LD (LB.IntoRegister H) (LB.FromRegister E) @ 4

        0x64 ->
            LD (LB.IntoRegister H) (LB.FromRegister H) @ 4

        0x65 ->
            LD (LB.IntoRegister H) (LB.FromRegister L) @ 4

        0x66 ->
            LD (LB.IntoRegister H) (LB.FromMem HL) @ 8

        0x67 ->
            LD (LB.IntoRegister H) (LB.FromRegister A) @ 4

        0x68 ->
            LD (LB.IntoRegister L) (LB.FromRegister B) @ 4

        0x69 ->
            LD (LB.IntoRegister L) (LB.FromRegister C) @ 4

        0x6A ->
            LD (LB.IntoRegister L) (LB.FromRegister D) @ 4

        0x6B ->
            LD (LB.IntoRegister L) (LB.FromRegister E) @ 4

        0x6C ->
            LD (LB.IntoRegister L) (LB.FromRegister H) @ 4

        0x6D ->
            LD (LB.IntoRegister L) (LB.FromRegister L) @ 4

        0x6E ->
            LD (LB.IntoRegister L) (LB.FromMem HL) @ 8

        0x6F ->
            LD (LB.IntoRegister L) (LB.FromRegister A) @ 4

        0x70 ->
            LD (LB.IntoMem HL) (LB.FromRegister B) @ 8

        0x71 ->
            LD (LB.IntoMem HL) (LB.FromRegister C) @ 8

        0x72 ->
            LD (LB.IntoMem HL) (LB.FromRegister D) @ 8

        0x73 ->
            LD (LB.IntoMem HL) (LB.FromRegister E) @ 8

        0x74 ->
            LD (LB.IntoMem HL) (LB.FromRegister H) @ 8

        0x75 ->
            LD (LB.IntoMem HL) (LB.FromRegister L) @ 8

        0x76 ->
            HALT @ 4

        0x77 ->
            LD (LB.IntoMem HL) (LB.FromRegister A) @ 8

        0x78 ->
            LD (LB.IntoRegister A) (LB.FromRegister B) @ 4

        0x79 ->
            LD (LB.IntoRegister A) (LB.FromRegister C) @ 4

        0x7A ->
            LD (LB.IntoRegister A) (LB.FromRegister D) @ 4

        0x7B ->
            LD (LB.IntoRegister A) (LB.FromRegister E) @ 4

        0x7C ->
            LD (LB.IntoRegister A) (LB.FromRegister H) @ 4

        0x7D ->
            LD (LB.IntoRegister A) (LB.FromRegister L) @ 4

        0x7E ->
            LD (LB.IntoRegister A) (LB.FromMem HL) @ 8

        0x7F ->
            LD (LB.IntoRegister A) (LB.FromRegister A) @ 8

        0x80 ->
            ADD (WithRegister B) @ 4

        0x81 ->
            ADD (WithRegister C) @ 4

        0x82 ->
            ADD (WithRegister D) @ 4

        0x83 ->
            ADD (WithRegister E) @ 4

        0x84 ->
            ADD (WithRegister H) @ 4

        0x85 ->
            ADD (WithRegister L) @ 4

        0x86 ->
            ADD WithMemHL @ 8

        0x87 ->
            ADD (WithRegister A) @ 4

        0x88 ->
            ADC A (WithRegister B) @ 4

        0x89 ->
            ADC A (WithRegister C) @ 4

        0x8A ->
            ADC A (WithRegister D) @ 4

        0x8B ->
            ADC A (WithRegister E) @ 4

        0x8C ->
            ADC A (WithRegister H) @ 4

        0x8D ->
            ADC A (WithRegister L) @ 4

        0x8E ->
            ADC A WithMemHL @ 8

        0x8F ->
            ADC A (WithRegister A) @ 4

        0x90 ->
            SUB (WithRegister B) @ 4

        0x91 ->
            SUB (WithRegister C) @ 4

        0x92 ->
            SUB (WithRegister D) @ 4

        0x93 ->
            SUB (WithRegister E) @ 4

        0x94 ->
            SUB (WithRegister H) @ 4

        0x95 ->
            SUB (WithRegister L) @ 4

        0x96 ->
            SUB WithMemHL @ 8

        0x97 ->
            SUB (WithRegister A) @ 4

        0x98 ->
            SBC (WithRegister B) @ 4

        0x99 ->
            SBC (WithRegister C) @ 4

        0x9A ->
            SBC (WithRegister D) @ 4

        0x9B ->
            SBC (WithRegister E) @ 4

        0x9C ->
            SBC (WithRegister H) @ 4

        0x9D ->
            SBC (WithRegister L) @ 4

        0x9E ->
            SBC WithMemHL @ 8

        0x9F ->
            SBC (WithRegister A) @ 4

        0xA0 ->
            AND (WithRegister B) @ 4

        0xA1 ->
            AND (WithRegister C) @ 4

        0xA2 ->
            AND (WithRegister D) @ 4

        0xA3 ->
            AND (WithRegister E) @ 4

        0xA4 ->
            AND (WithRegister H) @ 4

        0xA5 ->
            AND (WithRegister L) @ 4

        0xA6 ->
            AND WithMemHL @ 8

        0xA7 ->
            AND (WithRegister A) @ 4

        0xA8 ->
            XOR (WithRegister B) @ 4

        0xA9 ->
            XOR (WithRegister C) @ 4

        0xAA ->
            XOR (WithRegister D) @ 4

        0xAB ->
            XOR (WithRegister E) @ 4

        0xAC ->
            XOR (WithRegister H) @ 4

        0xAD ->
            XOR (WithRegister L) @ 4

        0xAE ->
            XOR WithMemHL @ 8

        0xAF ->
            XOR (WithRegister A) @ 4

        0xB0 ->
            OR (WithRegister B) @ 4

        0xB1 ->
            OR (WithRegister C) @ 4

        0xB2 ->
            OR (WithRegister D) @ 4

        0xB3 ->
            OR (WithRegister E) @ 4

        0xB4 ->
            OR (WithRegister H) @ 4

        0xB5 ->
            OR (WithRegister L) @ 4

        0xB6 ->
            OR WithMemHL @ 8

        0xB7 ->
            OR (WithRegister A) @ 4

        0xB8 ->
            CP (WithRegister B) @ 4

        0xB9 ->
            CP (WithRegister C) @ 4

        0xBA ->
            CP (WithRegister D) @ 4

        0xBB ->
            CP (WithRegister E) @ 4

        0xBC ->
            CP (WithRegister H) @ 4

        0xBD ->
            CP (WithRegister L) @ 4

        0xBE ->
            CP WithMemHL @ 8

        0xBF ->
            CP (WithRegister A) @ 4

        0xC0 ->
            RET (NotSet Zero) / ( 20, 8 )

        0xC1 ->
            POP BC @ 12

        0xC2 ->
            JP (NotSet Zero) JumpData / ( 16, 12 )

        0xC3 ->
            JP NoCondition JumpData / ( 16, 12 )

        0xC4 ->
            CALL (NotSet Zero) / ( 24, 12 )

        0xC5 ->
            PUSH BC @ 16

        0xC6 ->
            ADD WithData @ 8

        0xC7 ->
            RST 0x00 @ 16

        0xC8 ->
            RET (Set Zero) / ( 20, 8 )

        0xC9 ->
            RET NoCondition @ 16

        0xCA ->
            JP (Set Zero) JumpData / ( 16, 12 )

        0xCB ->
            PREFIX_CB @ 4

        0xCC ->
            CALL (Set Zero) / ( 24, 12 )

        0xCD ->
            CALL NoCondition @ 24

        0xCE ->
            ADC A WithData @ 8

        0xCF ->
            RST 0x08 @ 16

        0xD0 ->
            RET (NotSet Carry) / ( 20, 8 )

        0xD1 ->
            POP DE @ 12

        0xD2 ->
            JP (NotSet Carry) JumpData / ( 16, 12 )

        0xD3 ->
            none

        0xD4 ->
            CALL (NotSet Carry) / ( 24, 12 )

        0xD5 ->
            PUSH DE @ 16

        0xD6 ->
            SUB WithData @ 8

        0xD7 ->
            RST 0x10 @ 16

        0xD8 ->
            RET (Set Carry) / ( 20, 8 )

        0xD9 ->
            RETI @ 16

        0xDA ->
            JP (Set Carry) JumpData / ( 16, 12 )

        0xDB ->
            none

        0xDC ->
            CALL (Set Carry) / ( 24, 12 )

        0xDD ->
            none

        0xDE ->
            SBC (WithRegister A) @ 8

        0xDF ->
            RST 0x18 @ 16

        0xE0 ->
            LDH LO.IntoMemDataOffset LO.FromRegisterA @ 12

        0xE1 ->
            POP HL @ 12

        0xE2 ->
            LDH LO.IntoMemCOffset LO.FromRegisterA @ 8

        0xE3 ->
            none

        0xE4 ->
            none

        0xE5 ->
            PUSH HL @ 16

        0xE6 ->
            AND WithData @ 8

        0xE7 ->
            RST 0x20 @ 16

        0xE8 ->
            ADDSP @ 16

        0xE9 ->
            JP NoCondition JumpMemHL @ 4

        0xEA ->
            LD LB.IntoMemData (LB.FromRegister A) @ 16

        0xEB ->
            none

        0xEC ->
            none

        0xED ->
            none

        0xEE ->
            XOR WithData @ 8

        0xEF ->
            RST 0x28 @ 16

        0xF0 ->
            LDH LO.IntoRegisterA LO.FromMemDataOffset @ 12

        0xF1 ->
            POP AF @ 12

        0xF2 ->
            LDH LO.IntoRegisterA LO.FromMemCOffset @ 8

        0xF3 ->
            DI @ 4

        0xF4 ->
            none

        0xF5 ->
            PUSH AF @ 16

        0xF6 ->
            OR WithData @ 8

        0xF7 ->
            RST 0x30 @ 16

        0xF8 ->
            LDW (LW.IntoRegister HL) LW.FromSPByteData @ 12

        0xF9 ->
            LDW (LW.IntoRegister SP) (LW.FromRegister HL) @ 8

        0xFA ->
            LD LB.IntoMemData (LB.FromRegister A) @ 16

        0xFB ->
            EI @ 4

        0xFC ->
            none

        0xFD ->
            none

        0xFE ->
            CP WithData @ 8

        0xFF ->
            RST 0x38 @ 16

        b ->
            INVALID b @ 0


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
            case (Bitwise.and code 0x0F) % 8 of
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
        op param @ cycles


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
    NONE @ 0


{-| Returns a tuple containing the operation and number of
cycles it will take.
-}
(@) : Op -> Int -> ( Op, Cycles )
(@) op cycles =
    ( op, Always cycles )


{-| Same as `@` but for branching instructions. Also takes a
tuple instead of just an `Int`, where the first is if the
branch is taken, and the second if it is not.
-}
(/) : Op -> ( Int, Int ) -> ( Op, Cycles )
(/) op ( taken, notTaken ) =
    ( op, Branching taken notTaken )
