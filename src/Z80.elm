module Z80 exposing (..)

import Byte exposing (Byte)
import Memory exposing (Memory)
import Word exposing (Word)
import Z80.Registers exposing (..)


{-| An emulation of a Z80 CPU core
-}
type alias State =
    { clock : Clock
    , registers : Registers
    , memory : Memory
    }


type alias Clock =
    { m : Int
    , t : Int
    }


type alias Registers =
    { a : Byte
    , b : Byte
    , c : Byte
    , d : Byte
    , e : Byte
    , h : Byte
    , l : Byte
    , f : Byte
    , pc : Word {- 16-bit registers -}
    , sp : Word
    }
