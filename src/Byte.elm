module Byte
    exposing
        ( Byte
        , Result
        , add
        , addc
        , and
        , dec
        , fromInt
        , getBit
        , hasCarry
        , hasHalfCarry
        , inc
        , incc
        , isZero
        , lsbSet
        , msbSet
        , or
        , reset
        , resultToByte
        , resultToInt
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


{-| Opaque type representing an 8-bit number.
-}
type Byte
    = Byte Int


{-| Opaque type representing the result of an arithmetic operation.
-}
type Result
    = Result
        { byte : Byte
        , carry : Bool
        , halfCarry : Bool
        }


{-| Converts a `Result` to a `Byte`.
-}
resultToByte : Result -> Byte
resultToByte (Result r) =
    r.byte


{-| Converts a `Result` to an `Int`.
-}
resultToInt : Result -> Int
resultToInt (Result r) =
    toInt r.byte


{-| Returns `True` if there was a carry from the resulting operation.
-}
hasCarry : Result -> Bool
hasCarry (Result r) =
    r.carry


{-| Returns `True` if there was a half carry from the resulting
operation.
-}
hasHalfCarry : Result -> Bool
hasHalfCarry (Result r) =
    r.halfCarry


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

Guaranteed to be in the range: 0 <= n < 2^8
-}
toInt : Byte -> Int
toInt (Byte b) =
    b


{-| Adds two `Byte`s.
-}
add : Byte -> Byte -> Byte
add a b =
    resultToByte <| addc a b


{-| Bitwise and two `Byte`s.
-}
and : Byte -> Byte -> Byte
and (Byte a) (Byte b) =
    Byte <| Bitwise.and a b


{-| Bitwise or two `Byte`s.
-}
or : Byte -> Byte -> Byte
or (Byte a) (Byte b) =
    Byte <| Bitwise.or a b


{-| Adds two `Byte`s, returning a tuple of the sum and two `Bool`s indicating
if there was carry and half carry, respectively.
-}
addc : Byte -> Byte -> Result
addc (Byte x) (Byte y) =
    let
        sum =
            x + y
    in
        Result
            { byte = fromInt sum
            , carry = sum > 255
            , halfCarry = (Bitwise.and ((maskLower x) + (maskLower y)) 0x10 > 0)
            }


{-| Subtracts the second `Byte` from the first.
-}
sub : Byte -> Byte -> Byte
sub a b =
    Tuple.second <| subc a b


{-| Subtracts the second `Byte` from the first, returning a
tuple of the difference and a `Bool` indicating if there was
underflow.

    subc (fromInt 5) (fromInt 3) == ( False, fromInt 2 )
    subc (fromInt 2) (fromInt 3) == ( True, fromInt 255 )
-}
subc : Byte -> Byte -> ( Bool, Byte )
subc (Byte x) (Byte y) =
    let
        diff =
            x - y
    in
        ( diff < 0
        , fromInt <| diff + 256
        )


{-| Increment a Byte.
-}
inc : Byte -> Byte
inc byte =
    add byte <| fromInt 1


{-| Increment a Byte, also returning a `Bool` indicating if there was a half-carry.
-}
incc : Byte -> Result
incc byte =
    addc byte <| fromInt 1


{-| Decrement a Byte.
-}
dec : Byte -> Byte
dec byte =
    add byte <| fromInt -1


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
        |> mask
        |> Byte


{-| Shifts Byte left once.
-}
shiftLeft : Byte -> Byte
shiftLeft =
    shiftLeftBy 1


{-| Shifts Byte left n times.
-}
shiftLeftBy : Int -> Byte -> Byte
shiftLeftBy n (Byte b) =
    Bitwise.shiftLeftBy n b
        |> mask
        |> Byte


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
