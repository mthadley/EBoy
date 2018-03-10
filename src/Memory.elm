module Memory
    exposing
        ( Memory
        , init
        , initFromInts
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


{-| Numer of memory locations
-}
memSize : Int
memSize =
    65536


{-| Initializes zero-filled memory.
-}
init : Memory
init =
    Memory <| Array.repeat memSize <| Byte.fromInt 0


{-| Initializes memory from a list of Int. Useful for testing.
-}
initFromInts : List Int -> Memory
initFromInts ints =
    let
        rest =
            Array.repeat
                (min (List.length ints) memSize)
                (Byte.fromInt 0)
    in
    ints
        |> List.take memSize
        |> List.map Byte.fromInt
        |> Array.fromList
        |> flip Array.append rest
        |> Memory


{-| Reads a `Byte` from memory. Takes a `Word` indicating
the memory location to read from.
-}
readByte : Word -> Memory -> Byte
readByte addr (Memory array) =
    Word.toInt addr
        |> get array
        |> Maybe.withDefault (Byte.fromInt 0)


{-| Reads a `Word` from memory. Takes a `Word` indicating
the memory location to read from.
-}
readWord : Word -> Memory -> Word
readWord addr memory =
    Word.fromBytes
        (readByte (Word.inc addr) memory)
        (readByte addr memory)


{-| Writes a `Byte` to memory. Takes a `Word` indicating
the memory location to write to.
-}
writeByte : Word -> Byte -> Memory -> Memory
writeByte addr val (Memory array) =
    Memory <| Array.set (Word.toInt addr) val array


{-| Writes a `Word` to memory. Takes a `Word` indicating
the memory location to write to.
-}
writeWord : Word -> Word -> Memory -> Memory
writeWord addr val memory =
    let
        ( high, low ) =
            Word.toBytes val
    in
    memory
        |> writeByte addr low
        |> writeByte (Word.inc addr) high


get : Array a -> Int -> Maybe a
get =
    flip Array.get
