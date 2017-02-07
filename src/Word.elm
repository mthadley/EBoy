module Word
    exposing
        ( Word
        , fromInt
        , toInt
        , fromByte
        , fromBytes
        , toBytes
        , add
        , addc
        , sub
        , inc
        , dec
        )

import Bitwise
import Byte exposing (Byte)


{-| Opaque type representing a 16-bit word.
-}
type Word
    = Word Int


{-| Converts an `Int` to a `Word`.

    fromInt 1200 : Word
-}
fromInt : Int -> Word
fromInt =
    Word << mask


{-| Converts a `Byte` to a `Word`.
-}
fromByte : Byte -> Word
fromByte =
    Word << Byte.toInt


{-| Converts two `Byte`s to a `Word`. The first `Byte`
represents the higher 8 bits, and the second the lower
8 bits.
-}
fromBytes : Byte -> Byte -> Word
fromBytes high low =
    Bitwise.shiftLeftBy 8 (Byte.toInt high)
        |> Bitwise.or (Byte.toInt low)
        |> Word


{-| Converts a `Word` to a tuple of `Byte`s. The first
`Byte` represents the 8 high bits, and the second represents
the lower 8 bits.
-}
toBytes : Word -> ( Byte, Byte )
toBytes (Word w) =
    ( Byte.fromInt <| Bitwise.shiftRightZfBy 8 w
    , Byte.fromInt w
    )


{-| Adds two `Word`s.
-}
add : Word -> Word -> Word
add a b =
    let
        ( _, _, result ) =
            addc a b
    in
        result


{-| Adds two `Word`s, and returns a tuple where the the first two
booleans represent whether there was a carry and a half carry, respectively.
-}
addc : Word -> Word -> ( Bool, Bool, Word )
addc (Word a) (Word b) =
    let
        result =
            a + b
    in
        ( result > 0xFFFF
        , Bitwise.and ((maskLower a) + (maskLower b)) 0x1000 > 0
        , fromInt result
        )


{-| Subtracts the second `Word` from the first.
-}
sub : Word -> Word -> Word
sub (Word w) (Word x) =
    fromInt <| w - x


{-| Increments a `Word`.
-}
inc : Word -> Word
inc (Word w) =
    fromInt <| w + 1


{-| Decrements a `Word`.
-}
dec : Word -> Word
dec (Word w) =
    fromInt <| w - 1


{-| Converts a `Word` back to an `Int`.
-}
toInt : Word -> Int
toInt (Word w) =
    w


mask : Int -> Int
mask =
    Bitwise.and 0xFFFF


maskLower : Int -> Int
maskLower =
    Bitwise.and 0x0FFF
