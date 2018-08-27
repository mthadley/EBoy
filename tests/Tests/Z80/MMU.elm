module Tests.Z80.MMU exposing (invalidROM, loadROM, mbc1ROM, mbc2ROM, mockROM, ramDisableByte, readByte, suite, writeByte)

import Byte exposing (Byte)
import Expect
import Fuzz exposing (Fuzzer, int)
import Test exposing (Test, describe, fuzz, test)
import Tests.Util exposing (byte, expectOk)
import Word
import Z80.MMU as MMU exposing (MMU)


suite : Test
suite =
    describe "MMU"
        [ describe "readByte"
            [ test "Should read a byte from ROM" <|
                \_ ->
                    MMU.init
                        |> MMU.readByte Word.zero
                        |> Expect.equal Byte.zero
            , fuzz byte "Should be a shadow copy of Work RAM" <|
                \val ->
                    MMU.init
                        |> writeByte 0xC001 val
                        |> readByte 0xE001
                        |> Expect.equal val
            ]
        , describe "loadROM"
            [ test "Should successfully load a ROM" <|
                \_ ->
                    MMU.init
                        |> MMU.loadROM mbc1ROM
                        |> expectOk
            , test "Should fail to load an unsupported ROM" <|
                \_ ->
                    MMU.init
                        |> MMU.loadROM invalidROM
                        |> Expect.err
            , fuzz byte "Should read a byte from a loaded ROM" <|
                \val ->
                    MMU.init
                        |> loadROM [ val ]
                        |> readByte 0x00
                        |> Expect.equal val
            , test "Should read the correct rom type: ROM Only" <|
                \_ ->
                    MMU.init
                        |> loadROM [ Byte.zero ]
                        |> .romType
                        |> Expect.equal MMU.ROMOnly
            , test "Should read the correct rom type: MBC1" <|
                \_ ->
                    MMU.init
                        |> loadROM mbc1ROM
                        |> .romType
                        |> Expect.equal (MMU.MBC1 MMU.ROMBankMode)
            ]
        , describe "writeByte"
            [ describe "MBC1"
                [ fuzz byte "ERAM should start disabled" <|
                    \val ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0xA001 val
                            |> readByte 0xA001
                            |> Expect.equal (Byte.fromInt 0)
                , test "Should select ROM bank 1 by writing a 0" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 0)
                            |> .bankOffset
                            |> Expect.equal Byte.zero
                , test "Should select ROM bank 1 by writing a 1" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 1)
                            |> .bankOffset
                            |> Expect.equal Byte.zero
                , test "Should select ROM bank 2 by writing a 2" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 2)
                            |> .bankOffset
                            |> Expect.equal (Byte.fromInt 1)
                , test "Should select RAM bank 2" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x6000 (Byte.fromInt 1)
                            |> writeByte 0x4000 (Byte.fromInt 2)
                            |> .extRamOffset
                            |> Expect.equal (Byte.fromInt 2)
                , test "Accessing bank 0x21 accesses 0x20 instead" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 0x21)
                            |> writeByte 0x4000 (Byte.fromInt 0x01)
                            |> .bankOffset
                            |> Expect.equal (Byte.fromInt 0x20)
                , test "Accessing bank 0x41 accesses 0x40 instead" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 0x41)
                            |> writeByte 0x4000 (Byte.fromInt 0x02)
                            |> .bankOffset
                            |> Expect.equal (Byte.fromInt 0x40)
                , test "Accessing bank 0x61 accesses 0x60 instead" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc1ROM
                            |> writeByte 0x2000 (Byte.fromInt 0x61)
                            |> writeByte 0x4000 (Byte.fromInt 0x03)
                            |> .bankOffset
                            |> Expect.equal (Byte.fromInt 0x60)
                ]
            , describe "MBC2"
                [ test "Should select ROM bank 3" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc2ROM
                            |> writeByte 0x2100 (Byte.fromInt 0x03)
                            |> .bankOffset
                            |> Expect.equal (Byte.fromInt 0x03)
                , test "Should still have bank 1 selected" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc2ROM
                            |> writeByte 0x2100 (Byte.fromInt 0x20)
                            |> .bankOffset
                            |> Expect.equal Byte.zero
                , test "Should not set bank because lsb of higher byte not set" <|
                    \_ ->
                        MMU.init
                            |> loadROM mbc2ROM
                            |> writeByte 0x2200 (Byte.fromInt 0x03)
                            |> .bankOffset
                            |> Expect.equal Byte.zero
                ]
            ]
        , describe "romWrite"
            [ describe "MBC1"
                [ test "Should enable external RAM" <|
                    \_ ->
                        MMU.romWrite
                            (MMU.MBC1 MMU.ROMBankMode)
                            (Word.fromInt 0x1EEE)
                            MMU.ramEnableVal
                            |> Expect.equal (MMU.RAMEnable True)
                , fuzz ramDisableByte "Should disable external RAM" <|
                    MMU.romWrite (MMU.MBC1 MMU.ROMBankMode) (Word.fromInt 0x1EEE)
                        >> Expect.equal (MMU.RAMEnable False)
                ]
            , describe "MBC2"
                [ test "Should enable external RAM" <|
                    \_ ->
                        MMU.romWrite MMU.MBC2 Word.zero MMU.ramEnableVal
                            |> Expect.equal (MMU.RAMEnable True)
                , fuzz ramDisableByte "Should disable external RAM" <|
                    MMU.romWrite MMU.MBC2 Word.zero
                        >> Expect.equal (MMU.RAMEnable False)
                , test "Should not enable or disable external RAM" <|
                    \_ ->
                        MMU.romWrite MMU.MBC2 (Word.fromInt 0x0100) MMU.ramEnableVal
                            |> Expect.equal MMU.Noop
                ]
            ]
        ]


readByte : Int -> MMU -> Byte
readByte addr =
    MMU.readByte <| Word.fromInt addr


writeByte : Int -> Byte -> MMU -> MMU
writeByte addr =
    MMU.writeByte
        (Word.fromInt addr)


mockROM : Int -> List Byte
mockROM romType =
    List.append
        (List.repeat 0x0147 Byte.zero)
        [ Byte.fromInt romType ]


mbc1ROM : List Byte
mbc1ROM =
    mockROM 0x01


mbc2ROM : List Byte
mbc2ROM =
    mockROM 0x05


invalidROM : List Byte
invalidROM =
    mockROM 0x14


loadROM : List Byte -> MMU -> MMU
loadROM codes =
    Result.withDefault MMU.init << MMU.loadROM codes


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
