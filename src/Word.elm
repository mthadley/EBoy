module Word
    exposing
        ( Word
        , add
        , addc
        , dec
        , decc
        , fromByte
        , fromBytes
        , fromInt
        , inc
        , incc
        , isZero
        , subc
        , toBytes
        , toInt
        )

import Bitwise
import Carry exposing (Carry)
import Byte exposing (Byte)


{-| Opaque type representing a 16-bit word.
-}
type Word
    = Word Int


{-| Returns `True` if the `Byte` is zero.
-}
isZero : Word -> Bool
isZero =
    (==) 0 << toInt


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


{-| Converts two `Byte`s to a `Word`. The first `Byte` represents the higher 8
bits, and the second the lower 8 bits.
-}
fromBytes : Byte -> Byte -> Word
fromBytes high low =
    Bitwise.shiftLeftBy 8 (Byte.toInt high)
        |> Bitwise.or (Byte.toInt low)
        |> Word


{-| Converts a `Word` to a tuple of `Byte`s. The first `Byte` represents the 8
high bits, and the second represents the lower 8 bits.
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
    Carry.value <| addc a b


{-| Adds two `Word`s, and returns a `Carry`.
-}
addc : Word -> Word -> Carry Word
addc (Word a) (Word b) =
    let
        sum =
            a + b

        halfCarry =
            Bitwise.and ((maskLower a) + (maskLower b)) 0x1000 > 0
    in
        Carry.create
            (fromInt sum)
            (sum > 0xFFFF)
            (halfCarry)


{-| Subtracts the second `Word` from the first.
-}
subc : Word -> Word -> Carry Word
subc (Word w) (Word x) =
    Carry.create
        (fromInt <| w - x)
        (w < x)
        (maskLower w < maskLower x)


{-| Increments a `Word`, returning a `Carry`.
-}
incc : Word -> Carry Word
incc w =
    addc w <| fromInt 1


{-| Increments a `Word`.
-}
inc : Word -> Word
inc =
    Carry.value << incc


{-| Decrements a `Word`, returning a `Carry`.
-}
decc : Word -> Carry Word
decc w =
    subc w <| fromInt 1


{-| Decrements a `Word`.
-}
dec : Word -> Word
dec =
    Carry.value << decc


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
