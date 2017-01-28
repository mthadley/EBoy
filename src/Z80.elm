module Z80 exposing (..)

{-| An emulation of a Z80 core
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


type OpTarget
    = IntoRegister Register
    | IntoMemRegister Register
    | IntoMemData OpData


type OpSource
    = FromRegister Register
    | FromMemRegister Register
    | FromMemData OpData
    | Data OpData


type Op
    = NOP
    | LD OpTarget OpSource
    | INC Register
    | DEC Register
    | RLCA
    | ADD Register OpSource
    | RRCA
    | STOP


{-| Decodes an instruction, returning a tuple of the `Op` and
    how many cycles it will take.
-}
decode : Int -> ( Op, Int )
decode code =
    case code of
        0x00 ->
            NOP @ 4

        0x01 ->
            LD (IntoRegister BC) (Data D16) @ 12

        0x02 ->
            LD (IntoMemRegister BC) (FromRegister A) @ 8

        0x03 ->
            INC BC @ 8

        0x04 ->
            INC B @ 4

        0x05 ->
            DEC B @ 4

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
            DEC BC @ 8

        0x0C ->
            INC C @ 4

        0x0D ->
            DEC C @ 4

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
            INC DE @ 8

        0x14 ->
            INC D @ 4

        invalid ->
            Debug.crash <| toString invalid


{-| Returns a tuple containing the operation and number of
    cycles it will take.
-}
(@) : Op -> Int -> ( Op, Int )
(@) =
    (,)
