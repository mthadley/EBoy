module Z80.LW exposing (..)

import Z80.Registers exposing (WordRegister, ByteRegister)


{-| Targets for 16-bit `LD` instructions.
-}
type Target
    = IntoRegister WordRegister
    | IntoMemData


{-| Sources for 16-bit `LD` instructions.
-}
type Source
    = FromRegister WordRegister
    | FromData
    | FromSPByteData
