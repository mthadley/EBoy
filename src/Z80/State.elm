module Z80.State exposing (..)

import Byte exposing (Byte)
import Memory exposing (Memory)
import Word exposing (Word)


{-| An emulation of a Z80 CPU core
-}
type alias State =
    { clock : Int
    , memory : Memory
    , a : Byte
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


init : State
init =
    { clock = 0
    , memory = Memory.init
    , a = Byte.fromInt 0
    , b = Byte.fromInt 0
    , c = Byte.fromInt 0
    , d = Byte.fromInt 0
    , e = Byte.fromInt 0
    , h = Byte.fromInt 0
    , l = Byte.fromInt 0
    , f = Byte.fromInt 0
    , pc = Word.fromInt 0
    , sp = Word.fromInt 0
    }
