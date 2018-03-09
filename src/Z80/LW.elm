module Z80.LW exposing (..)

import Basics.Extra exposing ((=>))
import Carry
import Word exposing (Word)
import Z80.Flag as Flag
import Z80.Registers exposing (..)
import Z80.State exposing (..)


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


readSource : Source -> State -> ( Word, State )
readSource source state =
    case source of
        FromRegister register ->
            ( readWordRegister register state, state )

        FromData ->
            readDataWord state

        FromSPByteData ->
            let
                ( byte, newState ) =
                    readDataByte state

                result =
                    Word.addc
                        (readWordRegister SP state)
                        (Word.fromByte byte)
            in
            ( Carry.value result
            , setFlagsWith
                [ Flag.Carry => Carry.check result
                , Flag.HalfCarry => Carry.checkHalf result
                ]
                state
            )


writeTarget : Target -> ( Word, State ) -> State
writeTarget target ( word, state ) =
    case target of
        IntoRegister register ->
            writeWordRegister register state word

        IntoMemData ->
            let
                ( addr, newState ) =
                    readDataWord state
            in
            writeMemWord addr word newState
