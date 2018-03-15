module Test.Z80.MMU exposing (..)

import Byte exposing (Byte)
import Expect
import Fuzz exposing (Fuzzer, int)
import Test exposing (Test, describe, fuzz, test)
import Test.Util exposing (byte)
import Word
import Z80.MMU as MMU exposing (MMU)


suite : Test
suite =
    describe "MMU"
        [ describe "readByte"
            [ test "Should read a byte from ROM" <|
                \_ ->
                    MMU.init
                        |> MMU.readByte (Word.fromInt 0x00)
                        |> Expect.equal (Byte.fromInt 0)
            , fuzz byte "Should be a shadow copy of Work RAM" <|
                \val ->
                    MMU.init
                        |> writeByte 0xC001 val
                        |> readByte 0xE001
                        |> Expect.equal val
            ]
        , describe "loadRom"
            [ fuzz byte "Should read a byte from a loaded ROM" <|
                \val ->
                    MMU.init
                        |> MMU.loadRom [ val ]
                        |> readByte 0x00
                        |> Expect.equal val
            , test "Should read the correct rom type: ROM Only" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom [ Byte.fromInt 0x00 ]
                        |> .romType
                        |> Expect.equal MMU.ROMOnly
            , test "Should read the correct rom type: MBC1" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> .romType
                        |> Expect.equal (MMU.MBC1 MMU.ROMBankMode)
            ]
        , describe "writeByte"
            [ fuzz byte "ERAM should start disabled" <|
                \val ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0xA001 val
                        |> readByte 0xA001
                        |> Expect.equal (Byte.fromInt 0)
            , test "Should select ROM bank 1 by writing a 0" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 0)
                        |> .bankOffset
                        |> Expect.equal Byte.zero
            , test "Should select ROM bank 1 by writing a 1" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 1)
                        |> .bankOffset
                        |> Expect.equal Byte.zero
            , test "Should select ROM bank 2 by writing a 2" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 2)
                        |> .bankOffset
                        |> Expect.equal (Byte.fromInt 1)
            , test "Should select RAM bank 2" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x6000 (Byte.fromInt 1)
                        |> writeByte 0x4000 (Byte.fromInt 2)
                        |> .extRamOffset
                        |> Expect.equal (Byte.fromInt 2)
            , test "Accessing bank 0x21 accesses 0x20 instead" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 0x21)
                        |> writeByte 0x4000 (Byte.fromInt 0x01)
                        |> .bankOffset
                        |> Expect.equal (Byte.fromInt 0x20)
            , test "Accessing bank 0x41 accesses 0x40 instead" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 0x41)
                        |> writeByte 0x4000 (Byte.fromInt 0x02)
                        |> .bankOffset
                        |> Expect.equal (Byte.fromInt 0x40)
            , test "Accessing bank 0x61 accesses 0x60 instead" <|
                \_ ->
                    MMU.init
                        |> MMU.loadRom mbc1ROM
                        |> writeByte 0x2000 (Byte.fromInt 0x61)
                        |> writeByte 0x4000 (Byte.fromInt 0x03)
                        |> .bankOffset
                        |> Expect.equal (Byte.fromInt 0x60)
            ]
        , describe "romWrite"
            [ test "Should enable external RAM" <|
                \_ ->
                    MMU.romWrite
                        (MMU.MBC1 MMU.ROMBankMode)
                        0x1EEE
                        MMU.ramEnableVal
                        |> Expect.equal (MMU.RAMEnable True)
            , fuzz ramDisableByte "Should disable external RAM" <|
                MMU.romWrite (MMU.MBC1 MMU.ROMBankMode) 0x1EEE
                    >> Expect.equal (MMU.RAMEnable False)
            ]
        ]


readByte : Int -> MMU -> Byte
readByte addr =
    MMU.readByte <| Word.fromInt addr


writeByte : Int -> Byte -> MMU -> MMU
writeByte addr =
    MMU.writeByte
        (Word.fromInt addr)


mbc1ROM : List Byte
mbc1ROM =
    List.append
        (List.repeat 0x0147 (Byte.fromInt 0x00))
        [ Byte.fromInt 0x01 ]


ramDisableByte : Fuzzer Byte
ramDisableByte =
    Fuzz.map
        (\b ->
            if b == MMU.ramEnableVal then
                Byte.inc b
            else
                b
        )
        byte
