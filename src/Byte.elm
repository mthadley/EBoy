module Byte
    exposing
        ( Byte
        , fromInt
        , add
        , addc
        , sub
        , subc
        , lsbSet
        , msbSet
        )

import Bitwise


type Byte
    = Byte Int


{-| Converts an `Int` to a `Byte`.

    fromInt 3 == Byte 3
-}
fromInt : Int -> Byte
fromInt =
    Byte << mod


{-| Adds two `Byte`s.
-}
add : Byte -> Byte -> Byte
add a b =
    Tuple.second <| addc a b


{-| Adds two `Byte`s, returning a tuple of the sum and a
`Bool` indicating whether or not there was overflow.

    addc (Byte.fromInt 5) (Byte.fromInt 3) == ( False, Byte 8 )
    addc (Byte.fromInt 254) (Byte.fromInt 3) == ( True, Byte 2 )
-}
addc : Byte -> Byte -> ( Bool, Byte )
addc (Byte x) (Byte y) =
    let
        sum =
            x + y
    in
        ( sum > 255, Byte <| mod sum )


{-| Subtracts the second `Byte` from the first.
-}
sub : Byte -> Byte -> Byte
sub a b =
    Tuple.second <| subc a b


{-| Subtracts the second `Byte` from the first, returning a
tuple of the difference and a `Bool` indicating if there was
underflow.

    subc (Byte.fromInt 5) (Byte.fromInt 3) == ( False, Byte 2 )
    subc (Byte.fromInt 2) (Byte.fromInt 3) == ( True, Byte 255 )
-}
subc : Byte -> Byte -> ( Bool, Byte )
subc (Byte x) (Byte y) =
    let
        diff =
            x - y
    in
        ( diff < 0
        , Byte <| mod <| diff + 256
        )


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


{-| Returns a  `Bool` indicating wether or not the bit is set.
-}
getBit : Int -> Byte -> Bool
getBit n (Byte b) =
    (Bitwise.and 1 <| Bitwise.shiftRightBy n b) == 1


mod : Int -> Int
mod n =
    n % 256
