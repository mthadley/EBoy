module Util exposing (..)

{-| Various utility funcitons.
-}


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
