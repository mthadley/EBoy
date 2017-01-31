module Z80.LO exposing (..)

{-| Types for `LDH` instructions.
-}


{-| Sources for `LDH` instructions.
-}
type Source
    = FromRegisterA
    | FromMemDataOffset
    | FromMemCOffset


{-| Targets for `LDH` instructions.
-}
type Target
    = IntoRegisterA
    | IntoMemDataOffset
    | IntoMemCOffset
