module Z80.LO exposing (..)

import Z80.Registers exposing (..)
import Byte exposing (Byte)
import Z80.State exposing (..)


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


readSource : Source -> State -> ( Byte, State )
readSource source state =
    case source of
        FromRegisterA ->
            ( readByteRegister A state, state )

        FromMemDataOffset ->
            readMemDataOffset state

        FromMemCOffset ->
            ( readMemRegisterOffset C state, state )


writeTarget : Target -> ( Byte, State ) -> State
writeTarget target ( byte, state ) =
    case target of
        IntoRegisterA ->
            writeByteRegister A state byte

        IntoMemDataOffset ->
            let
                ( word, newState ) =
                    Tuple.mapFirst wordOffset <| readDataByte state
            in
                writeMemByte word byte newState

        IntoMemCOffset ->
            let
                addr =
                    wordOffset <| readByteRegister C state
            in
                writeMemByte addr byte state
