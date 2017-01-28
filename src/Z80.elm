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


type Register
    = A
    | B
    | C
    | D
    | E
    | H
    | L
    | F
    | PC
    | SP
    | M
    | T
    | BC
    | HL
    | DE


{-| OpData
    * d8  means immediate 8 bit data
    * d16 means immediate 16 bit data
    * a8  means 8 bit unsigned data, which are added to $FF00 in
          certain instructions (replacement for missing IN and OUT instructions)
    * a16 means 16 bit address
    * r8  means 8 bit signed data, which are added to program counter
-}
type OpData
    = D8
    | D16
    | A8
    | A16
    | R8


{-| Targets for `LD` instructions.
-}
type LoadTarget
    = IntoRegister Register
    | IntoMemRegister Register
    | IntoMemData OpData


{-| Sourcesfor `LD` instructions.
-}
type LoadSource
    = FromRegister Register
    | FromMemRegister Register
    | FromMemData OpData
    | Data OpData


{-| Param for arithmetic operations (`ADD`, `SUB`, etc.).
-}
type Param
    = WithRegister Register
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
    = NOP
    | LD LoadTarget LoadSource
    | INC Param
    | DEC Param
    | RLCA
    | ADD Register LoadSource
    | RRCA
    | STOP
    | RLA
    | JR FlagCondition
    | RRA
    | LDI LoadTarget LoadSource
    | DAA
    | CPL
    | LDD LoadTarget LoadSource
    | SCF


{-| Decodes an instruction, returning a tuple of the `Op` and
    how many `Cycle`'s' it will take.
-}
decode : Int -> ( Op, Cycles )
decode code =
    case code of
        0x00 ->
            NOP @ 4

        0x01 ->
            LD (IntoRegister BC) (Data D16) @ 12

        0x02 ->
            LD (IntoMemRegister BC) (FromRegister A) @ 8

        0x03 ->
            INC (WithRegister BC) @ 8

        0x04 ->
            INC (WithRegister B) @ 4

        0x05 ->
            DEC (WithRegister B) @ 4

        0x06 ->
            LD (IntoRegister B) (Data D8) @ 8

        0x07 ->
            RLCA @ 4

        0x08 ->
            LD (IntoMemData A16) (FromRegister SP) @ 20

        0x09 ->
            ADD HL (FromRegister BC) @ 8

        0x0A ->
            LD (IntoRegister A) (FromMemRegister BC) @ 8

        0x0B ->
            DEC (WithRegister BC) @ 8

        0x0C ->
            INC (WithRegister C) @ 4

        0x0D ->
            DEC (WithRegister C) @ 4

        0x0E ->
            LD (IntoRegister C) (Data D8) @ 8

        0x0F ->
            RRCA @ 4

        0x10 ->
            STOP @ 4

        0x11 ->
            LD (IntoRegister DE) (Data D16) @ 12

        0x12 ->
            LD (IntoMemRegister DE) (FromRegister A) @ 8

        0x13 ->
            INC (WithRegister DE) @ 8

        0x14 ->
            INC (WithRegister D) @ 4

        0x15 ->
            DEC (WithRegister D) @ 4

        0x16 ->
            LD (IntoRegister D) (Data D8) @ 8

        0x17 ->
            RLA @ 4

        0x18 ->
            JR NoCondition @ 12

        0x19 ->
            ADD HL (FromRegister DE) @ 8

        0x1A ->
            LD (IntoRegister A) (FromMemRegister DE) @ 8

        0x1B ->
            DEC (WithRegister DE) @ 8

        0x1C ->
            INC (WithRegister E) @ 4

        0x1D ->
            DEC (WithRegister E) @ 4

        0x1E ->
            LD (IntoRegister E) (Data D8) @ 8

        0x1F ->
            RRA @ 4

        0x20 ->
            JR (NotSet Zero) / ( 12, 8 )

        0x21 ->
            LD (IntoRegister HL) (Data D16) @ 12

        0x22 ->
            LDI (IntoMemRegister HL) (FromRegister A) @ 8

        0x23 ->
            INC (WithRegister HL) @ 8

        0x24 ->
            INC (WithRegister H) @ 4

        0x25 ->
            DEC (WithRegister H) @ 4

        0x26 ->
            LD (IntoRegister H) (Data D8) @ 8

        0x27 ->
            DAA @ 4

        0x28 ->
            JR (Set Zero) / ( 12, 8 )

        0x29 ->
            ADD HL (FromRegister HL) @ 8

        0x2A ->
            LDI (IntoRegister A) (FromMemRegister HL) @ 8

        0x2B ->
            DEC (WithRegister HL) @ 8

        0x2C ->
            INC (WithRegister L) @ 4

        0x2D ->
            DEC (WithRegister L) @ 4

        0x2E ->
            LD (IntoRegister L) (Data D8) @ 8

        0x2F ->
            CPL @ 4

        0x30 ->
            JR (NotSet Carry) / ( 12, 8 )

        0x31 ->
            LD (IntoRegister SP) (Data D16) @ 12

        0x32 ->
            LDD (IntoMemRegister HL) (FromRegister A) @ 8

        0x33 ->
            INC (WithRegister SP) @ 8

        0x34 ->
            INC WithMemHL @ 12

        0x35 ->
            DEC WithMemHL @ 12

        0x36 ->
            LD (IntoMemRegister HL) (Data D8) @ 12

        0x37 ->
            SCF @ 4

        0x38 ->
            JR (Set Carry) / ( 12, 8 )

        invalid ->
            Debug.crash <| toString invalid


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
