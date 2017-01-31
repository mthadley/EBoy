module Z80.Registers exposing (..)


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
