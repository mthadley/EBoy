module Test.Z80.MMU exposing (..)

import Byte
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz, test)
import Word
import Z80.MMU as MMU


suite : Test
suite =
    describe "MMU"
        [ describe "readByte"
            [ test "Should read a byte from ROM" <|
                \_ ->
                    MMU.init
                        |> MMU.readByte (Word.fromInt 0x00)
                        |> Expect.equal (Byte.fromInt 0)
            , fuzz int "Should be a shadow copy of Work RAM" <|
                \val ->
                    MMU.init
                        |> MMU.writeByte
                            (Word.fromInt 0xC001)
                            (Byte.fromInt val)
                        |> MMU.readByte (Word.fromInt 0xE001)
                        |> Expect.equal (Byte.fromInt val)
            ]
        , describe "loadRom"
            [ fuzz int "Should read a byte from a loaded ROM" <|
                \val ->
                    MMU.init
                        |> MMU.loadRom [ Byte.fromInt val ]
                        |> MMU.readByte (Word.fromInt 0x00)
                        |> Expect.equal (Byte.fromInt val)
            ]
        ]
