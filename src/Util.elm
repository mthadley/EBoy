module Util exposing (clone, cloneWith, toInt)

{-| Various utility funcitons.
-}


{-| Converts a `Bool` to an `Int`.
-}
toInt : Bool -> Int
toInt bool =
    if bool then
        1

    else
        0


{-| Returns a tuple of a value after passing it through a function.
-}
cloneWith : (a -> b) -> a -> ( b, b )
cloneWith f =
    clone << f


{-| Returns a tuple of a value.
-}
clone : a -> ( a, a )
clone a =
    ( a, a )
