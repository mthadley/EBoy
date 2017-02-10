module Byte
    exposing
        ( Byte
        , add
        , addc
        , and
        , dec
        , decc
        , fromInt
        , getBit
        , inc
        , incc
        , isZero
        , lsbSet
        , msbSet
        , or
        , reset
        , rotateLeft
        , rotateLeftBy
        , rotateRight
        , rotateRightBy
        , set
        , shiftLeft
        , shiftLeftBy
        , shiftRight
        , shiftRightBy
        , shiftRightZf
        , shiftRightZfBy
        , sub
        , subc
        , toInt
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
        (maskLower x < maskLower y)


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


{-| Rotate byte left.
-}
rotateLeft : Byte -> Byte
rotateLeft =
    rotateLeftBy 1


{-| Rotate byte right.
-}
rotateRight : Byte -> Byte
rotateRight =
    rotateRightBy 1


{-| Rotate byte left N times.
-}
rotateLeftBy : Int -> Byte -> Byte
rotateLeftBy =
    rotate True


{-| Rotate byte right N times.
-}
rotateRightBy : Int -> Byte -> Byte
rotateRightBy =
    rotate False


{-| Sets the nth bit of the `Byte`.
-}
set : Int -> Byte -> Byte
set n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> mask
        |> Bitwise.or b
        |> Byte


{-| Resets the nth bit of the `Byte`.
-}
reset : Int -> Byte -> Byte
reset n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> Bitwise.complement
        |> Bitwise.and b
        |> fromInt


{-| Shifts Byte left once.
-}
shiftLeft : Byte -> Byte
shiftLeft =
    shiftLeftBy 1


{-| Shifts Byte left n times.
-}
shiftLeftBy : Int -> Byte -> Byte
shiftLeftBy n (Byte b) =
    fromInt <| Bitwise.shiftLeftBy n b


{-| Shifts Byte right once, preserving sign.
-}
shiftRight : Byte -> Byte
shiftRight =
    shiftRightBy 1


{-| Shifts Byte right n times, preserving sign.
-}
shiftRightBy : Int -> Byte -> Byte
shiftRightBy n ((Byte b) as byte) =
    let
        sign =
            if msbSet byte then
                Bitwise.complement 0
                    |> Bitwise.shiftLeftBy (8 - n)
                    |> mask
            else
                0
    in
        sign
            |> Bitwise.or (Bitwise.shiftRightZfBy n b)
            |> Byte


{-| Shifts Byte right once, filling with zeroes.
-}
shiftRightZf : Byte -> Byte
shiftRightZf =
    shiftRightZfBy 1


{-| Shifts Byte right n times, filling with zeroes.
-}
shiftRightZfBy : Int -> Byte -> Byte
shiftRightZfBy n (Byte b) =
    Byte <| Bitwise.shiftRightZfBy n b


rotate : Bool -> Int -> Byte -> Byte
rotate left n (Byte b) =
    let
        ( leftTimes, rightTimes ) =
            if left then
                ( n, 8 - n )
            else
                ( 8 - n, n )
    in
        Byte <|
            Bitwise.or
                (mask <| Bitwise.shiftLeftBy leftTimes b)
                (Bitwise.shiftRightZfBy rightTimes b)


mask : Int -> Int
mask =
    Bitwise.and 0xFF


maskLower : Int -> Int
maskLower =
    Bitwise.and 0x0F
