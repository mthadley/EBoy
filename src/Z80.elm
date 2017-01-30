module Z80 exposing (..)

{-| An emulation of a Z80 CPU core
    TODO: Possible remove `ByteData` and `WordData`, these might be
    meaningless.
-}


type alias State =
    { clock : Clock
    , registers : Registers
    }


type alias Clock =
    { m : Int
    , t : Int
    }


type alias Registers =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    , h : Int
    , l : Int
    , f : Int
    , pc : Int {- 16-bit registers -}
    , sp : Int
    , m : Int {- Clock -}
    , t : Int
    }


type ByteRegister
    = A
    | B
    | C
    | D
    | E
    | H
    | L
    | F
    | M
    | T


type WordRegister
    = PC
    | SP
    | BC
    | HL
    | DE
    | AF


{-| ByteData
    * d8  means immediate 8 bit data
    * a8  means 8 bit unsigned data, which are added to $FF00 in
          certain instructions (replacement for missing IN and OUT instructions)
    * r8  means 8 bit signed data, which are added to program counter
-}
type ByteData
    = D8
    | A8
    | R8


{-| WordData
    * d16 means immediate 16 bit data
    * a16 means 16 bit address
-}
type WordData
    = D16
    | A16


{-| Targets for 8-bit `LD` instructions.
-}
type LoadByteTarget
    = IntoByteRegister ByteRegister
    | IntoMem WordRegister
    | IntoMemWordData


{-| Sources for 8-bit `LD` instructions.
-}
type LoadByteSource
    = FromByteRegister ByteRegister
    | FromMem WordRegister
    | FromByteData ByteData


{-| Targets for 16-bit `LD` instructions.
-}
type LoadWordTarget
    = IntoWordRegister WordRegister
    | IntoMemData WordData


{-| Sources for 16-bit `LD` instructions.
-}
type LoadWordSource
    = FromWordRegister WordRegister
    | FromWordData WordData
    | FromSPByteData


{-| Sources for `LDH` instructions.
-}
type LoadOffsetSource
    = FromRegisterA
    | FromMemDataOffset
    | FromMemCOffset


{-| Targets for `LDH` instructions.
-}
type LoadOffsetTarget
    = IntoRegisterA
    | IntoMemDataOffset
    | IntoMemCOffset


{-| Jump Targets
-}
type JumpTarget
    = JumpData WordData
    | JumpMemHL


{-| Param for arithmetic operations (`ADD`, `SUB`, etc.).
-}
type Param
    = WithRegister ByteRegister
    | WithMemHL
    | WithData ByteData


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
    | LD LoadByteTarget LoadByteSource
    | LDH LoadOffsetTarget LoadOffsetSource
    | LDW LoadWordTarget LoadWordSource
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
    | LDI LoadByteTarget LoadByteSource
    | DAA
    | CPL
    | LDD LoadByteTarget LoadByteSource
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


{-| Decodes an instruction, returning a tuple of the `Op` and
    how many `Cycle`'s' it will take.
-}
decode : Int -> ( Op, Cycles )
decode code =
    case code of
        0x01 ->
            LDW (IntoWordRegister BC) (FromWordData D16) @ 12

        0x02 ->
            LD (IntoMem BC) (FromByteRegister A) @ 8

        0x03 ->
            INCW BC @ 8

        0x04 ->
            INC (WithRegister B) @ 4

        0x05 ->
            DEC (WithRegister B) @ 4

        0x06 ->
            LD (IntoByteRegister B) (FromByteData D8) @ 8

        0x07 ->
            RLCA @ 4

        0x08 ->
            LDW (IntoMemData A16) (FromWordRegister SP) @ 20

        0x09 ->
            ADDW HL BC @ 8

        0x0A ->
            LD (IntoByteRegister A) (FromMem BC) @ 8

        0x0B ->
            DECW BC @ 8

        0x0C ->
            INC (WithRegister C) @ 4

        0x0D ->
            DEC (WithRegister C) @ 4

        0x0E ->
            LD (IntoByteRegister C) (FromByteData D8) @ 8

        0x0F ->
            RRCA @ 4

        0x10 ->
            STOP @ 4

        0x11 ->
            LDW (IntoWordRegister DE) (FromWordData D16) @ 12

        0x12 ->
            LD (IntoMem DE) (FromByteRegister A) @ 8

        0x13 ->
            INCW DE @ 8

        0x14 ->
            INC (WithRegister D) @ 4

        0x15 ->
            DEC (WithRegister D) @ 4

        0x16 ->
            LD (IntoByteRegister D) (FromByteData D8) @ 8

        0x17 ->
            RLA @ 4

        0x18 ->
            JR NoCondition @ 12

        0x19 ->
            ADDW HL DE @ 8

        0x1A ->
            LD (IntoByteRegister A) (FromMem DE) @ 8

        0x1B ->
            DECW DE @ 8

        0x1C ->
            INC (WithRegister E) @ 4

        0x1D ->
            DEC (WithRegister E) @ 4

        0x1E ->
            LD (IntoByteRegister E) (FromByteData D8) @ 8

        0x1F ->
            RRA @ 4

        0x20 ->
            JR (NotSet Zero) / ( 12, 8 )

        0x21 ->
            LDW (IntoWordRegister HL) (FromWordData D16) @ 12

        0x22 ->
            LDI (IntoMem HL) (FromByteRegister A) @ 8

        0x23 ->
            INCW HL @ 8

        0x24 ->
            INC (WithRegister H) @ 4

        0x25 ->
            DEC (WithRegister H) @ 4

        0x26 ->
            LD (IntoByteRegister H) (FromByteData D8) @ 8

        0x27 ->
            DAA @ 4

        0x28 ->
            JR (Set Zero) / ( 12, 8 )

        0x29 ->
            ADDW HL HL @ 8

        0x2A ->
            LDI (IntoByteRegister A) (FromMem HL) @ 8

        0x2B ->
            DECW HL @ 8

        0x2C ->
            INC (WithRegister L) @ 4

        0x2D ->
            DEC (WithRegister L) @ 4

        0x2E ->
            LD (IntoByteRegister L) (FromByteData D8) @ 8

        0x2F ->
            CPL @ 4

        0x30 ->
            JR (NotSet Carry) / ( 12, 8 )

        0x31 ->
            LDW (IntoWordRegister SP) (FromWordData D16) @ 12

        0x32 ->
            LDD (IntoMem HL) (FromByteRegister A) @ 8

        0x33 ->
            INCW SP @ 8

        0x34 ->
            INC WithMemHL @ 12

        0x35 ->
            DEC WithMemHL @ 12

        0x36 ->
            LD (IntoMem HL) (FromByteData D8) @ 12

        0x37 ->
            SCF @ 4

        0x38 ->
            JR (Set Carry) / ( 12, 8 )

        0x39 ->
            ADDW HL SP @ 8

        0x3A ->
            LDD (IntoByteRegister A) (FromMem HL) @ 8

        0x3B ->
            DECW SP @ 8

        0x3C ->
            INC (WithRegister A) @ 4

        0x3D ->
            DEC (WithRegister A) @ 4

        0x3E ->
            LD (IntoByteRegister A) (FromByteData D8) @ 8

        0x3F ->
            CCF @ 4

        0x40 ->
            LD (IntoByteRegister B) (FromByteRegister B) @ 4

        0x41 ->
            LD (IntoByteRegister B) (FromByteRegister C) @ 4

        0x42 ->
            LD (IntoByteRegister B) (FromByteRegister D) @ 4

        0x43 ->
            LD (IntoByteRegister B) (FromByteRegister E) @ 4

        0x44 ->
            LD (IntoByteRegister B) (FromByteRegister H) @ 4

        0x45 ->
            LD (IntoByteRegister B) (FromByteRegister L) @ 4

        0x46 ->
            LD (IntoByteRegister B) (FromMem HL) @ 8

        0x47 ->
            LD (IntoByteRegister B) (FromByteRegister A) @ 4

        0x48 ->
            LD (IntoByteRegister C) (FromByteRegister B) @ 4

        0x49 ->
            LD (IntoByteRegister C) (FromByteRegister C) @ 4

        0x4A ->
            LD (IntoByteRegister C) (FromByteRegister D) @ 4

        0x4B ->
            LD (IntoByteRegister C) (FromByteRegister E) @ 4

        0x4C ->
            LD (IntoByteRegister C) (FromByteRegister H) @ 4

        0x4D ->
            LD (IntoByteRegister C) (FromByteRegister L) @ 4

        0x4E ->
            LD (IntoByteRegister C) (FromMem HL) @ 8

        0x4F ->
            LD (IntoByteRegister C) (FromByteRegister A) @ 4

        0x50 ->
            LD (IntoByteRegister D) (FromByteRegister B) @ 4

        0x51 ->
            LD (IntoByteRegister D) (FromByteRegister C) @ 4

        0x52 ->
            LD (IntoByteRegister D) (FromByteRegister D) @ 4

        0x53 ->
            LD (IntoByteRegister D) (FromByteRegister E) @ 4

        0x54 ->
            LD (IntoByteRegister D) (FromByteRegister H) @ 4

        0x55 ->
            LD (IntoByteRegister D) (FromByteRegister L) @ 4

        0x56 ->
            LD (IntoByteRegister D) (FromMem HL) @ 8

        0x57 ->
            LD (IntoByteRegister D) (FromByteRegister A) @ 4

        0x58 ->
            LD (IntoByteRegister E) (FromByteRegister B) @ 4

        0x59 ->
            LD (IntoByteRegister E) (FromByteRegister C) @ 4

        0x5A ->
            LD (IntoByteRegister E) (FromByteRegister D) @ 4

        0x5B ->
            LD (IntoByteRegister E) (FromByteRegister E) @ 4

        0x5C ->
            LD (IntoByteRegister E) (FromByteRegister H) @ 4

        0x5D ->
            LD (IntoByteRegister E) (FromByteRegister L) @ 4

        0x5E ->
            LD (IntoByteRegister E) (FromMem HL) @ 8

        0x5F ->
            LD (IntoByteRegister E) (FromByteRegister A) @ 4

        0x60 ->
            LD (IntoByteRegister H) (FromByteRegister B) @ 4

        0x61 ->
            LD (IntoByteRegister H) (FromByteRegister C) @ 4

        0x62 ->
            LD (IntoByteRegister H) (FromByteRegister D) @ 4

        0x63 ->
            LD (IntoByteRegister H) (FromByteRegister E) @ 4

        0x64 ->
            LD (IntoByteRegister H) (FromByteRegister H) @ 4

        0x65 ->
            LD (IntoByteRegister H) (FromByteRegister L) @ 4

        0x66 ->
            LD (IntoByteRegister H) (FromMem HL) @ 8

        0x67 ->
            LD (IntoByteRegister H) (FromByteRegister A) @ 4

        0x68 ->
            LD (IntoByteRegister L) (FromByteRegister B) @ 4

        0x69 ->
            LD (IntoByteRegister L) (FromByteRegister C) @ 4

        0x6A ->
            LD (IntoByteRegister L) (FromByteRegister D) @ 4

        0x6B ->
            LD (IntoByteRegister L) (FromByteRegister E) @ 4

        0x6C ->
            LD (IntoByteRegister L) (FromByteRegister H) @ 4

        0x6D ->
            LD (IntoByteRegister L) (FromByteRegister L) @ 4

        0x6E ->
            LD (IntoByteRegister L) (FromMem HL) @ 8

        0x6F ->
            LD (IntoByteRegister L) (FromByteRegister A) @ 4

        0x70 ->
            LD (IntoMem HL) (FromByteRegister B) @ 8

        0x71 ->
            LD (IntoMem HL) (FromByteRegister C) @ 8

        0x72 ->
            LD (IntoMem HL) (FromByteRegister D) @ 8

        0x73 ->
            LD (IntoMem HL) (FromByteRegister E) @ 8

        0x74 ->
            LD (IntoMem HL) (FromByteRegister H) @ 8

        0x75 ->
            LD (IntoMem HL) (FromByteRegister L) @ 8

        0x76 ->
            HALT @ 4

        0x77 ->
            LD (IntoMem HL) (FromByteRegister A) @ 8

        0x78 ->
            LD (IntoByteRegister A) (FromByteRegister B) @ 4

        0x79 ->
            LD (IntoByteRegister A) (FromByteRegister C) @ 4

        0x7A ->
            LD (IntoByteRegister A) (FromByteRegister D) @ 4

        0x7B ->
            LD (IntoByteRegister A) (FromByteRegister E) @ 4

        0x7C ->
            LD (IntoByteRegister A) (FromByteRegister H) @ 4

        0x7D ->
            LD (IntoByteRegister A) (FromByteRegister L) @ 4

        0x7E ->
            LD (IntoByteRegister A) (FromMem HL) @ 8

        0x7F ->
            LD (IntoByteRegister A) (FromByteRegister A) @ 8

        0x80 ->
            ADD A (WithRegister B) @ 4

        0x81 ->
            ADD A (WithRegister C) @ 4

        0x82 ->
            ADD A (WithRegister D) @ 4

        0x83 ->
            ADD A (WithRegister E) @ 4

        0x84 ->
            ADD A (WithRegister H) @ 4

        0x85 ->
            ADD A (WithRegister L) @ 4

        0x86 ->
            ADD A WithMemHL @ 8

        0x87 ->
            ADD A (WithRegister A) @ 4

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
            JP (NotSet Zero) (JumpData A16) / ( 16, 12 )

        0xC3 ->
            JP NoCondition (JumpData A16) / ( 16, 12 )

        0xC4 ->
            CALL (NotSet Zero) / ( 24, 12 )

        0xC5 ->
            PUSH BC @ 16

        0xC6 ->
            ADD A (WithData D8) @ 8

        0xC7 ->
            RST 0x00 @ 16

        0xC8 ->
            RET (Set Zero) / ( 20, 8 )

        0xC9 ->
            RET NoCondition @ 16

        0xCA ->
            JP (Set Zero) (JumpData A16) / ( 16, 12 )

        0xCB ->
            PREFIX_CB @ 4

        0xCC ->
            CALL (Set Zero) / ( 24, 12 )

        0xCD ->
            CALL NoCondition @ 24

        0xCE ->
            ADC A (WithData D8) @ 8

        0xCF ->
            RST 0x08 @ 16

        0xD0 ->
            RET (NotSet Carry) / ( 20, 8 )

        0xD1 ->
            POP DE @ 12

        0xD2 ->
            JP (NotSet Carry) (JumpData A16) / ( 16, 12 )

        0xD3 ->
            none

        0xD4 ->
            CALL (NotSet Carry) / ( 24, 12 )

        0xD5 ->
            PUSH DE @ 16

        0xD6 ->
            SUB (WithData D8) @ 8

        0xD7 ->
            RST 0x10 @ 16

        0xD8 ->
            RET (Set Carry) / ( 20, 8 )

        0xD9 ->
            RETI @ 16

        0xDA ->
            JP (Set Carry) (JumpData A16) / ( 16, 12 )

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
            LDH IntoMemDataOffset FromRegisterA @ 12

        0xE1 ->
            POP HL @ 12

        0xE2 ->
            LDH IntoMemCOffset FromRegisterA @ 8

        0xE3 ->
            none

        0xE4 ->
            none

        0xE5 ->
            PUSH HL @ 16

        0xE6 ->
            AND (WithData D8) @ 8

        0xE7 ->
            RST 0x20 @ 16

        0xE8 ->
            ADDSP @ 16

        0xE9 ->
            JP NoCondition JumpMemHL @ 4

        0xEA ->
            LD IntoMemWordData (FromByteRegister A) @ 16

        0xEB ->
            none

        0xEC ->
            none

        0xED ->
            none

        0xEE ->
            XOR (WithData D8) @ 8

        0xEF ->
            RST 0x28 @ 16

        0xF0 ->
            LDH IntoRegisterA FromMemDataOffset @ 12

        0xF1 ->
            POP AF @ 12

        0xF2 ->
            LDH IntoRegisterA FromMemCOffset @ 8

        0xF3 ->
            DI @ 4

        0xF4 ->
            none

        0xF5 ->
            PUSH AF @ 16

        0xF6 ->
            OR (WithData D8) @ 8

        0xF7 ->
            RST 0x30 @ 16

        0xF8 ->
            LDW (IntoWordRegister HL) FromSPByteData @ 12

        0xF9 ->
            LDW (IntoWordRegister SP) (FromWordRegister HL) @ 8

        0xFA ->
            LD IntoMemWordData (FromByteRegister A) @ 16

        0xFB ->
            EI @ 4

        0xFC ->
            none

        0xFD ->
            none

        0xFE ->
            CP (WithData D8) @ 8

        0xFF ->
            RST 0x38 @ 16

        b ->
            INVALID b @ 0


none : ( Op, Cycles )
none =
    NONE @ 0


{-| Represents how many cycles an intruction will take. Some
    have two values since they are branching (`JR`, `JRNZ`, etc.).
-}
type Cycles
    = Always Int
    | Branching Int Int


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
