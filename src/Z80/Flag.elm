module Z80.Flag
    exposing
        ( Flag(..)
        , set
        , setWith
        , reset
        , setFlag
        , resetFlag
        , isSet
        )

import Byte exposing (Byte)


{-| The types of flags represented in the Flag (F) register.
-}
type Flag
    = Zero
    | Subtract
    | HalfCarry
    | Carry


{-| Sets the `Flag`s in the `Byte`.
-}
set : List Flag -> Byte -> Byte
set flags byte =
    List.foldr setFlag byte flags


{-| Takes a tuple list of `Flag`s and `Bool`, where the flag will be set
or reset based on the `Bool`.
-}
setWith : List ( Flag, Bool ) -> Byte -> Byte
setWith flags byte =
    let
        setOrReset ( flag, shouldSet ) byte =
            if shouldSet then
                setFlag flag byte
            else
                resetFlag flag byte
    in
        List.foldr setOrReset byte flags


{-| Resets the `Flag`s in the `Byte`.
-}
reset : List Flag -> Byte -> Byte
reset flags byte =
    List.foldr resetFlag byte flags


{-| Sets the `Flag` in the `Byte`.
-}
setFlag : Flag -> Byte -> Byte
setFlag flag =
    Byte.set <| flagBit flag


{-| Resets the `Flag` in the `Byte`.
-}
resetFlag : Flag -> Byte -> Byte
resetFlag flag =
    Byte.reset <| flagBit flag


{-| Returns `True` if the `Flag` is set in the `Byte`.
-}
isSet : Flag -> Byte -> Bool
isSet flag =
    Byte.getBit <| flagBit flag


flagBit : Flag -> Int
flagBit flag =
    case flag of
        Zero ->
            7

        Subtract ->
            6

        HalfCarry ->
            5

        Carry ->
            4
