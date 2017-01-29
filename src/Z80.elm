module Z80 exposing (..)

{-| An emulation of a Z80 CPU core
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


{-| Sources for 8-bit `LD` instructions.
-}
type LoadByteSource
    = FromByteRegister ByteRegister
    | FromMem WordRegister
    | ByteData ByteData


{-| Targets for 16-bit `LD` instructions.
-}
type LoadWordTarget
    = IntoWordRegister WordRegister
    | IntoMemData WordData


{-| Sources for 16-bit `LD` instructions.
-}
type LoadWordSource
    = SPPlusR8
    | FromWordRegister WordRegister
    | WordData WordData


{-| Param for arithmetic operations (`ADD`, `SUB`, etc.).
-}
type Param
    = WithRegister ByteRegister
    | WithMemHL


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


type Op
    = INVALID Int
    | NOP
    | LD LoadByteTarget LoadByteSource
    | LDW LoadWordTarget LoadWordSource
    | INC Param
    | INCW WordRegister
    | DEC Param
    | DECW WordRegister
    | RLCA
    | ADD ByteRegister Param
    | ADDW WordRegister WordRegister
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


{-| Decodes an instruction, returning a tuple of the `Op` and
    how many `Cycle`'s' it will take.
-}
decode : Int -> ( Op, Cycles )
decode code =
    case code of
        0x01 ->
            LDW (IntoWordRegister BC) (WordData D16) @ 12

        0x02 ->
            LD (IntoMem BC) (FromByteRegister A) @ 8

        0x03 ->
            INCW BC @ 8

        0x04 ->
            INC (WithRegister B) @ 4

        0x05 ->
            DEC (WithRegister B) @ 4

        0x06 ->
            LD (IntoByteRegister B) (ByteData D8) @ 8

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
            LD (IntoByteRegister C) (ByteData D8) @ 8

        0x0F ->
            RRCA @ 4

        0x10 ->
            STOP @ 4

        0x11 ->
            LDW (IntoWordRegister DE) (WordData D16) @ 12

        0x12 ->
            LD (IntoMem DE) (FromByteRegister A) @ 8

        0x13 ->
            INCW DE @ 8

        0x14 ->
            INC (WithRegister D) @ 4

        0x15 ->
            DEC (WithRegister D) @ 4

        0x16 ->
            LD (IntoByteRegister D) (ByteData D8) @ 8

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
            LD (IntoByteRegister E) (ByteData D8) @ 8

        0x1F ->
            RRA @ 4

        0x20 ->
            JR (NotSet Zero) / ( 12, 8 )

        0x21 ->
            LDW (IntoWordRegister HL) (WordData D16) @ 12

        0x22 ->
            LDI (IntoMem HL) (FromByteRegister A) @ 8

        0x23 ->
            INCW HL @ 8

        0x24 ->
            INC (WithRegister H) @ 4

        0x25 ->
            DEC (WithRegister H) @ 4

        0x26 ->
            LD (IntoByteRegister H) (ByteData D8) @ 8

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
            LD (IntoByteRegister L) (ByteData D8) @ 8

        0x2F ->
            CPL @ 4

        0x30 ->
            JR (NotSet Carry) / ( 12, 8 )

        0x31 ->
            LDW (IntoWordRegister SP) (WordData D16) @ 12

        0x32 ->
            LDD (IntoMem HL) (FromByteRegister A) @ 8

        0x33 ->
            INCW SP @ 8

        0x34 ->
            INC WithMemHL @ 12

        0x35 ->
            DEC WithMemHL @ 12

        0x36 ->
            LD (IntoMem HL) (ByteData D8) @ 12

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
            LD (IntoByteRegister A) (ByteData D8) @ 8

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

        val ->
            INVALID val @ 0


{-| Represents how many cycles an intruction will take. Some
    differ since they are branching (`JR`, `JRNZ`, etc.).
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
