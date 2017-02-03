module Z80 exposing (..)

import Array exposing (Array)
import Byte exposing (Byte)
import Z80.Registers exposing (..)


{-| An emulation of a Z80 CPU core
-}
type alias State =
    { clock : Clock
    , registers : Registers
    , memory : Array Byte
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
    , pc : Int {- 16-bit registers -}
    , sp : Int
    }
