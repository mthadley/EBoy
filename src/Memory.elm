module Memory
    exposing
        ( Memory
        , init
        , readByte
        , readWord
        , writeByte
        , writeWord
        )

import Array exposing (Array)
import Byte exposing (Byte)
import Word exposing (Word)


{-| An opaque type respresenting the state of memory. Note: The
Z80 is little-endian.
-}
type Memory
    = Memory (Array Byte)


{-| Initializes zero-filled memory.
-}
init : Memory
init =
    Memory <| Array.repeat 65536 <| Byte.fromInt 0


{-| Reads a `Byte` from memory. Takes a `Word` indicating
the memory location to read from.
-}
readByte : Word -> Memory -> Byte
readByte loc (Memory array) =
    Word.toInt loc
        |> get array
        |> Maybe.withDefault (Byte.fromInt 0)


{-| Reads a `Word` from memory. Takes a `Word` indicating
the memory location to read from.
-}
readWord : Word -> Memory -> Word
readWord loc memory =
    Word.fromBytes
        (readByte (Word.inc loc) memory)
        (readByte loc memory)


{-| Writes a `Byte` to memory. Takes a `Word` indicating
the memory location to write to.
-}
writeByte : Word -> Byte -> Memory -> Memory
writeByte loc val (Memory array) =
    Memory <| Array.set (Word.toInt loc) val array


{-| Writes a `Word` to memory. Takes a `Word` indicating
the memory location to write to.
-}
writeWord : Word -> Word -> Memory -> Memory
writeWord loc val memory =
    let
        ( high, low ) =
            Word.toBytes val
    in
        memory
            |> writeByte loc low
            |> writeByte (Word.inc loc) high


get : Array a -> Int -> Maybe a
get =
    flip Array.get
