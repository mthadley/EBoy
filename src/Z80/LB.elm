module Z80.LB exposing (..)

import Byte exposing (Byte)
import Z80.Registers exposing (..)
import Z80.State exposing (..)


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


readSource : Source -> State -> ( Byte, State )
readSource source state =
    case source of
        FromRegister register ->
            ( readByteRegister register state, state )

        FromMem register ->
            ( readMemRegister register state, state )

        FromData ->
            readDataByte state


writeTarget : Target -> ( Byte, State ) -> State
writeTarget target ( byte, state ) =
    case target of
        IntoRegister register ->
            writeByteRegister register state byte

        IntoMem register ->
            writeMemRegister register state byte

        IntoMemData ->
            let
                ( addr, newState ) =
                    readDataWord state
            in
            writeMemByte addr byte newState
