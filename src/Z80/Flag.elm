module Z80.Flag exposing
    ( Flag(..)
    , isSet
    , reset
    , resetEach
    , set
    , setEach
    , setWith
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
setEach : List Flag -> Byte -> Byte
setEach flags byte =
    List.foldr set byte flags


{-| Takes a tuple list of `Flag`s and `Bool`, where the flag will be set
or reset based on the `Bool`.
-}
setWith : List ( Flag, Bool ) -> Byte -> Byte
setWith flags currentByte =
    let
        setOrReset ( flag, shouldSet ) byte =
            if shouldSet then
                set flag byte

            else
                reset flag byte
    in
    List.foldr setOrReset currentByte flags


{-| Resets the `Flag`s in the `Byte`.
-}
resetEach : List Flag -> Byte -> Byte
resetEach flags byte =
    List.foldr reset byte flags


{-| Sets the `Flag` in the `Byte`.
-}
set : Flag -> Byte -> Byte
set flag =
    Byte.set <| flagBit flag


{-| Resets the `Flag` in the `Byte`.
-}
reset : Flag -> Byte -> Byte
reset flag =
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
