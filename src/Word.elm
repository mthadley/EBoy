module Word
    exposing
        ( Word
        , Result
        , add
        , addc
        , dec
        , fromByte
        , fromBytes
        , fromInt
        , hasCarry
        , hasHalfCarry
        , inc
        , isZero
        , resultToInt
        , resultToWord
        , sub
        , toBytes
        , toInt
        )

import Bitwise
import Byte exposing (Byte)


{-| Opaque type representing a 16-bit word.
-}
type Word
    = Word Int


{-| Opaque type representing the result of an arithmetic operation.
-}
type Result
    = Result
        { carry : Bool
        , halfCarry : Bool
        , word : Word
        }


{-| Converts a `Result` to a `Word`.
-}
resultToWord : Result -> Word
resultToWord (Result r) =
    r.word


{-| Converts a `Result` to an `Int`.
-}
resultToInt : Result -> Int
resultToInt (Result r) =
    toInt r.word


{-| Returns `True` if there was a carry from the resulting operation.
-}
hasCarry : Result -> Bool
hasCarry (Result r) =
    r.carry


{-| Returns `True` if the `Byte` is zero.
-}
isZero : Word -> Bool
isZero =
    (==) 0 << toInt


{-| Returns `True` if there was a half carry from the resulting
operation.
-}
hasHalfCarry : Result -> Bool
hasHalfCarry (Result r) =
    r.halfCarry


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
    resultToWord <| addc a b


{-| Adds two `Word`s, and returns a `Result`.
-}
addc : Word -> Word -> Result
addc (Word a) (Word b) =
    let
        sum =
            a + b

        halfCarry =
            Bitwise.and ((maskLower a) + (maskLower b)) 0x1000 > 0
    in
        Result
            { carry = sum > 0xFFFF
            , halfCarry = halfCarry
            , word = fromInt sum
            }


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
