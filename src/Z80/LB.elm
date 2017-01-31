module Z80.LB exposing (..)

import Z80.Registers exposing (WordRegister, ByteRegister)


{-| Targets for 8-bit `LD` instructions.
-}
type Target
    = IntoRegister ByteRegister
    | IntoMem WordRegister
    | IntoMemData


{-| Sources for 8-bit `LD` instructions.
-}
type Source
    = FromRegister ByteRegister
    | FromMem WordRegister
    | FromData
