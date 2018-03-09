module Z80.Registers exposing (..)

{-| The registers of the Z80.
-}


{-| A type respresenting the possible 8-bit registers.
-}
type ByteRegister
    = A
    | B
    | C
    | D
    | E
    | H
    | L
    | F


{-| A type respresenting the possible 16-bit registers. Only `PC`
and `SP` are _true_ 16-bit registers, the rest are pairs of 8-bit
registers.
-}
type WordRegister
    = PC
    | SP
    | BC
    | HL
    | DE
    | AF
