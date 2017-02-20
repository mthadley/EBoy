module Byte
    exposing
        ( Byte
        , add
        , addc
        , and
        , complement
        , dec
        , decc
        , fromInt
        , getBit
        , highNibble
        , inc
        , incc
        , isZero
        , lowNibble
        , lsbSet
        , msbSet
        , or
        , reset
        , rotateLeft
        , rotateRight
        , set
        , setWith
        , shiftLeft
        , shiftRight
        , shiftRightZf
        , sub
        , subc
        , swap
        , toInt
        , xor
        )

import Bitwise
import Carry exposing (Carry)


{-| Opaque type representing an 8-bit number.
-}
type Byte
    = Byte Int


{-| Returns `True` if the `Byte` is zero.
-}
isZero : Byte -> Bool
isZero =
    (==) 0 << toInt


{-| Converts an `Int` to a `Byte`.

    fromInt 3 : Byte
-}
fromInt : Int -> Byte
fromInt =
    Byte << mask


{-| Converts a `Byte` to an `Int`.

    (fromInt 3 |> toInt) == 3

Guaranteed to be in the range: `10 <= n < 2^8`
-}
toInt : Byte -> Int
toInt (Byte b) =
    b


{-| Adds two `Byte`s.
-}
add : Byte -> Byte -> Byte
add a b =
    Carry.value <| addc a b


{-| Bitwise and (&) two `Byte`s.
-}
and : Byte -> Byte -> Byte
and (Byte a) (Byte b) =
    Byte <| Bitwise.and a b


{-| Bitwise or (|) two `Byte`s.
-}
or : Byte -> Byte -> Byte
or (Byte a) (Byte b) =
    Byte <| Bitwise.or a b


{-| Bitwise xor two `Byte`s.
-}
xor : Byte -> Byte -> Byte
xor (Byte a) (Byte b) =
    Byte <| Bitwise.xor a b


{-| Adds two `Byte`s, returning a `Carry`.
-}
addc : Byte -> Byte -> Carry Byte
addc (Byte x) (Byte y) =
    let
        sum =
            x + y
    in
        Carry.create
            (fromInt sum)
            (sum > 255)
            (Bitwise.and ((maskLower x) + (maskLower y)) 0x10 > 0)


{-| Bitwise complement (Flips each bit).
-}
complement : Byte -> Byte
complement (Byte b) =
    fromInt <| Bitwise.complement b


{-| Subtracts the second `Byte` from the first.
-}
sub : Byte -> Byte -> Byte
sub a b =
    Carry.value <| subc a b


{-| Subtracts the second `Byte` from the first, returning a carry.

-}
subc : Byte -> Byte -> Carry Byte
subc (Byte x) (Byte y) =
    Carry.create
        (fromInt <| x - y)
        (x < y)
        ((maskHigher x) < (maskHigher y))


{-| Increment a Byte.
-}
inc : Byte -> Byte
inc byte =
    add byte <| fromInt 1


{-| Increment a `Byte`, returning a `Carry`.
-}
incc : Byte -> Carry Byte
incc byte =
    addc byte <| fromInt 1


{-| Decrement a `Byte`, returning a `Carry`.
-}
decc : Byte -> Carry Byte
decc byte =
    subc byte <| fromInt 1


{-| Decrement a `Byte`.
-}
dec : Byte -> Byte
dec =
    Carry.value << decc


{-| Returns a `Bool` indicating whether or not the most significant
bit is set.
-}
msbSet : Byte -> Bool
msbSet =
    getBit 7


{-| Returns a `Bool` indicating whether or not the least significant
bit is set.
-}
lsbSet : Byte -> Bool
lsbSet =
    getBit 0


{-| Returns a  `Bool` indicating whether or not the bit is set.
-}
getBit : Int -> Byte -> Bool
getBit n (Byte b) =
    (Bitwise.and 1 <| Bitwise.shiftRightBy n b) == 1


{-| Returns an `Int` respresenting the higher 4 bits.
-}
highNibble : Byte -> Int
highNibble (Byte b) =
    Bitwise.shiftRightZfBy 4 b


{-| Returns an `Int` respresenting the lower 4 bits.
-}
lowNibble : Byte -> Int
lowNibble (Byte b) =
    maskLower b


{-| Rotate byte left.
-}
rotateLeft : Byte -> Byte
rotateLeft =
    rotate Left


{-| Rotate byte right.
-}
rotateRight : Byte -> Byte
rotateRight =
    rotate Right


{-| Sets the nth bit of the `Byte`.
-}
set : Int -> Byte -> Byte
set n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> mask
        |> Bitwise.or b
        |> Byte


{-| Swaps the high and low nibbles of the `Byte`.
-}
swap : Byte -> Byte
swap byte =
    Byte <|
        (Bitwise.shiftLeftBy 4 <| lowNibble byte)
            + (highNibble byte)


{-| Resets the nth bit of the `Byte`.
-}
reset : Int -> Byte -> Byte
reset n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> Bitwise.complement
        |> Bitwise.and b
        |> fromInt


{-| Sets or resets the nth bit of the `Byte`.
-}
setWith : Int -> Bool -> Byte -> Byte
setWith n shouldSet byte =
    if shouldSet then
        set n byte
    else
        reset n byte


{-| Shifts Byte left.
-}
shiftLeft : Byte -> Carry Byte
shiftLeft ((Byte b) as byte) =
    Carry.create
        (fromInt <| Bitwise.shiftLeftBy 1 b)
        (msbSet byte)
        False


{-| Shifts Byte right, preserving sign.
-}
shiftRight : Byte -> Carry Byte
shiftRight ((Byte b) as byte) =
    let
        result =
            b
                |> Bitwise.shiftRightBy 1
                |> Byte
                |> setWith 7 (msbSet byte)
    in
        Carry.create result (lsbSet byte) False


{-| Shifts `Byte` right, filling with zeroes.
-}
shiftRightZf : Byte -> Carry Byte
shiftRightZf ((Byte b) as byte) =
    Carry.create
        (Byte <| Bitwise.shiftRightZfBy 1 b)
        (lsbSet byte)
        False


type Rotation
    = Left
    | Right


rotate : Rotation -> Byte -> Byte
rotate rotation (Byte b) =
    let
        ( leftTimes, rightTimes ) =
            case rotation of
                Left ->
                    ( 1, 7 )

                Right ->
                    ( 7, 1 )
    in
        fromInt <|
            Bitwise.or
                (mask <| Bitwise.shiftLeftBy leftTimes b)
                (Bitwise.shiftRightZfBy rightTimes b)


mask : Int -> Int
mask =
    Bitwise.and 0xFF


maskHigher : Int -> Int
maskHigher =
    Bitwise.and 0xF0


maskLower : Int -> Int
maskLower =
    Bitwise.and 0x0F
