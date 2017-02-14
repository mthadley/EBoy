module Carry
    exposing
        ( Carry
        , create
        , value
        , check
        , checkHalf
        , withoutOp
        )

{-| Opaque type representing the result of an operation and whether or not
there were any carries.
-}


type Carry a
    = Carry
        { value : a
        , carry : Bool
        , halfCarry : Bool
        }


{-| Creates a `Carry` with a value, a `Bool` indicating if there was a
carry, and a second `Bool` indicating if there was a half-carry.
-}
create : a -> Bool -> Bool -> Carry a
create value carry halfCarry =
    Carry
        { value = value
        , carry = carry
        , halfCarry = halfCarry
        }


{-| Used to create a `Carry` that was not a result of some kind of operation.
-}
withoutOp : a -> Carry a
withoutOp a =
    Carry
        { value = a
        , carry = False
        , halfCarry = False
        }


{-| Converts a `Carry` to it's value.
-}
value : Carry a -> a
value (Carry c) =
    c.value


{-| Returns `True` if there was a carry from the resulting operation.
-}
check : Carry a -> Bool
check (Carry c) =
    c.carry


{-| Returns `True` if there was a half carry from the resulting
operation.
-}
checkHalf : Carry a -> Bool
checkHalf (Carry c) =
    c.halfCarry
