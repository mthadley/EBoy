module Memory
    exposing
        ( Memory
        , init
        , initFromCodes
        , readByte
        , writeByte
        )

import Array exposing (Array)
import Byte exposing (Byte)


{-| An opaque type respresenting the state of memory. Note: The
Z80 is little-endian.
-}
type Memory
    = Memory (Array Byte)


{-| Initializes zero-filled memory.
-}
init : Int -> Memory
init size =
    Memory <| Array.repeat size <| Byte.fromInt 0


{-| Initializes memory from a list of integers.
-}
initFromCodes : List Byte -> Int -> Memory
initFromCodes ints size =
    let
        rest =
            Array.repeat
                (size - List.length ints)
                (Byte.fromInt 0)
    in
    ints
        |> List.take size
        |> Array.fromList
        |> flip Array.append rest
        |> Memory


{-| Reads a `Byte` from memory. Takes an `Int` indicating
the memory location to read from.
-}
readByte : Int -> Memory -> Byte
readByte addr (Memory array) =
    addr
        |> get array
        |> Maybe.withDefault (Byte.fromInt 0)


{-| Writes a `Byte` to memory. Takes an `Int` indicating
the memory location to write to.
-}
writeByte : Int -> Byte -> Memory -> Memory
writeByte addr val (Memory array) =
    Memory <| Array.set addr val array


get : Array a -> Int -> Maybe a
get =
    flip Array.get
