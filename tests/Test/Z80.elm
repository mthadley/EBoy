module Test.Z80 exposing (..)

import Basics.Extra exposing ((=>))
import Test exposing (Test, describe, test)
import Test.Util exposing (..)
import Z80.Flag exposing (Flag(..))
import Z80.Mode as Mode
import Z80.Registers exposing (..)


{-| Note: Work RAM starts at 0xC000
-}
tests : List Unit
tests =
    [ withCode [ 0x00 ]
        |> expectByte A 0x00
        |> expectByte B 0x00
        |> expectByte C 0x00
        |> expectByte D 0x00
        |> expectByte E 0x00
        |> expectByte H 0x00
        |> expectByte L 0x00
        |> expectByte F 0x00
        |> expectWord PC 0x01
        |> expectWord SP 0xFFFE
        |> expectWord BC 0x00
        |> expectWord HL 0x00
        |> expectWord DE 0x00
        |> expectWord AF 0x00
        |> expectFlags
            [ Carry => False
            , Zero => False
            , Subtract => False
            , HalfCarry => False
            ]
    , withCode [ 0x01, 0x01, 0x02 ]
        |> expectByte B 0x02
        |> expectByte C 0x01
        |> expectWord BC 0x0201
    , withCode [ 0x02 ]
        |> withByte A 0x23
        |> withWord BC 0xC042
        |> expectMem 0xC042 0x23
    , withCode [ 0x03 ]
        |> expectByte B 0
        |> expectByte C 1
        |> expectWord BC 1
    , withCode [ 0x04 ]
        |> expectByte B 1
    , withCode [ 0x05 ]
        |> expectByte B 255
    , withCode [ 0x06, 0x23 ]
        |> expectByte B 0x23
        |> expectWord PC 0x02
    , withCode [ 0x07, 0x23 ]
        |> withByte A 0x04
        |> expectByte A 0x08
    , withCode [ 0x08, 0x22, 0xC0 ]
        |> withWord SP 0x8888
        |> expectWord PC 0x03
        |> expectMemWord 0xC022 0x8888
    , withCode [ 0x09 ]
        |> withWord HL 0x0202
        |> expectWord BC 0x0202
    , withCode [ 0x0A ]
        |> withWord BC 0xD001
        |> withMem 0xD001 0x32
        |> expectByte A 0x32
    , withCode [ 0x0B ]
        |> withWord BC 0x06
        |> expectWord BC 0x05
    , withCode [ 0x0C ]
        |> withByte C 0x22
        |> expectByte C 0x23
    , withCode [ 0x0D ]
        |> withByte C 0x22
        |> expectByte C 0x21
    , withCode [ 0x0E, 0x24 ]
        |> expectByte C 0x24
    , withCode [ 0x0F ]
        |> withByte A 0x11
        |> expectByte A 0x88
        |> expectFlags [ Carry => True ]
    , withCode [ 0x10 ]
        |> expectMode Mode.Stopped
    , withCode [ 0x11, 0x44, 0x22 ]
        |> withWord DE 0x1111
        |> expectWord DE 0x2244
    , withCode [ 0x12 ]
        |> withWord DE 0xCFEE
        |> withByte A 0x23
        |> expectMem 0xCFEE 0x23
    , withCode [ 0x13 ]
        |> withWord DE 0x1111
        |> expectWord DE 0x1112
    , withCode [ 0x14 ]
        |> withByte D 0x02
        |> expectByte D 0x03
    , withCode [ 0x15 ]
        |> withByte D 0x04
        |> expectByte D 0x03
    , withCode [ 0x16, 0x32 ]
        |> expectByte D 0x32
    , withCode [ 0x17 ]
        |> withByte A 0x81
        |> expectByte A 0x02
        |> expectFlags [ Carry => True ]
    , withCode [ 0x18, 0x23 ]
        |> expectWord PC 0x25
    , withCode [ 0x19 ]
        |> withWord HL 0x2233
        |> withWord DE 0x4422
        |> expectWord DE 0x6655
    , withCode [ 0x1A ]
        |> withMem 0xC001 0x08
        |> withWord DE 0xC001
        |> expectByte A 0x08
    , withCode [ 0x1B ]
        |> withWord DE 0x03
        |> expectWord DE 0x02
    , withCode [ 0x1C ]
        |> withByte E 0x03
        |> expectByte E 0x04
    , withCode [ 0x1D ]
        |> withByte E 0x03
        |> expectByte E 0x02
    , withCode [ 0x1E, 0x03 ]
        |> expectByte E 0x03
    , withCode [ 0x1F ]
        |> withByte A 0x40
        |> expectByte A 0x20
    , withCode [ 0x20, 0x04 ]
        |> withFlags [ Zero => False ]
        |> expectWord PC 0x06
    , withCode [ 0x21, 0x04, 0x06 ]
        |> expectWord HL 0x0604
        |> expectWord PC 0x03
    , withCode [ 0x22 ]
        |> withByte A 0x07
        |> withWord HL 0xC001
        |> expectMem 0xC001 0x07
        |> expectWord HL 0xC002
    , withCode [ 0x23 ]
        |> withWord HL 0x2323
        |> expectWord HL 0x2324
    , withCode [ 0x24 ]
        |> expectByte H 0x01
    , withCode [ 0x25 ]
        |> expectByte H 0xFF
    , withCode [ 0x26, 0x07 ]
        |> expectByte H 0x07
    , withCode [ 0x27 ]
        |> withByte A 0x0F
        |> expectByte A 0x15
    , withCode [ 0x28, 0x04 ]
        |> withFlags [ Zero => True ]
        |> expectWord PC 0x06
    , withCode [ 0x29 ]
        |> withWord HL 0x06
        |> expectWord HL 0x0C
    , withCode [ 0x2A ]
        |> withWord HL 0xC001
        |> withMem 0xC001 0x32
        |> expectByte A 0x32
        |> expectWord HL 0xC002
    , withCode [ 0x2B ]
        |> expectWord HL 0xFFFF
        |> expectFlags
            [ Carry => False
            , Zero => False
            , Subtract => False
            , HalfCarry => False
            ]
    , withCode [ 0x2C ]
        |> expectByte L 0x01
    , withCode [ 0x2D ]
        |> expectByte L 0xFF
    , withCode [ 0x2E, 0x34 ]
        |> expectByte L 0x34
    , withCode [ 0x2F ]
        |> withByte A 0x55
        |> expectByte A 0xAA
    ]


suite : Test
suite =
    describe "Z80"
        [ describe "Basic Op code tests" <| List.map toTest tests
        , describe "RRCA"
            [ test "Should rotate through carry" <|
                (withCode [ 0x0F ]
                    |> withByte A 0x02
                    |> expectByte A 0x01
                    |> expectFlags
                        [ Carry => False
                        , Zero => False
                        , Subtract => False
                        , HalfCarry => False
                        ]
                    |> runTest
                )
            , test "Should always reset zero flag"
                (withCode [ 0x0F ]
                    |> withByte A 0x00
                    |> expectByte A 0x00
                    |> expectFlags
                        [ Carry => False
                        , Zero => False
                        , Subtract => False
                        , HalfCarry => False
                        ]
                    |> runTest
                )
            ]
        , describe "RLA"
            [ test "Should rotate through carry" <|
                (withCode [ 0x17 ]
                    |> withByte A 0x01
                    |> expectByte A 0x02
                    |> expectFlags
                        [ Carry => False
                        , Zero => False
                        ]
                    |> runTest
                )
            , test "Should set zero flag if result is zero"
                (withCode [ 0x17 ]
                    |> withByte A 0x80
                    |> expectByte A 0x00
                    |> expectFlags
                        [ Carry => True
                        , Zero => True
                        ]
                    |> runTest
                )
            ]
        , describe "RRA"
            [ test "Old 0 bit should be new carry"
                (withCode [ 0x1F ]
                    |> withByte A 0x01
                    |> expectByte A 0x00
                    |> expectFlags
                        [ Carry => True
                        , Zero => False
                        , Subtract => False
                        , HalfCarry => False
                        ]
                    |> runTest
                )
            , test "Carry should be new significant bit"
                (withCode [ 0x1F ]
                    |> withFlags [ Carry => True ]
                    |> expectByte A 0x80
                    |> expectFlags
                        [ Carry => False
                        , Zero => False
                        , Subtract => False
                        , HalfCarry => False
                        ]
                    |> runTest
                )
            ]
        , describe "JR"
            [ test "NZ, Should add signed data to PC"
                (withCode [ 0x00, 0x00, 0x00, 0x00, 0x20, 0x84 ]
                    |> withFlags [ Zero => False ]
                    |> withWord PC 0x04
                    |> expectWord PC 0x02
                    |> runTest
                )
            , test "NZ, Should not jump"
                (withCode [ 0x00, 0x00, 0x00, 0x00, 0x20, 0x84 ]
                    |> withFlags [ Zero => True ]
                    |> withWord PC 0x04
                    |> expectWord PC 0x06
                    |> runTest
                )
            , test "Z, Should add signed data to PC"
                (withCode [ 0x00, 0x00, 0x00, 0x00, 0x28, 0x84 ]
                    |> withFlags [ Zero => True ]
                    |> withWord PC 0x04
                    |> expectWord PC 0x02
                    |> runTest
                )
            , test "Z, Should not jump"
                (withCode [ 0x00, 0x00, 0x00, 0x00, 0x28, 0x84 ]
                    |> withFlags [ Zero => False ]
                    |> withWord PC 0x04
                    |> expectWord PC 0x06
                    |> runTest
                )
            ]
        , describe "DAA"
            [ test "should not adjust number, already BCD"
                (withCode [ 0x27 ]
                    |> withByte A 0x00
                    |> expectByte A 0x00
                    |> runTest
                )
            , test "should adjust number, not BCD"
                (withCode [ 0x27 ]
                    |> withByte A 0x0A
                    |> expectByte A 0x10
                    |> runTest
                )
            , test "should adjust higher nibble"
                (withCode [ 0x27 ]
                    |> withByte A 0xF0
                    |> expectByte A 0x50
                    |> runTest
                )
            , test "should not adjust high nibble, already BCD"
                (withCode [ 0x27 ]
                    |> withByte A 0x90
                    |> expectByte A 0x90
                    |> runTest
                )
            ]
        , describe "CPL"
            [ test "Should set correct flags"
                (withCode [ 0x2F ]
                    |> withByte A 0x55
                    |> expectByte A 0xAA
                    |> expectFlags
                        [ Subtract => True
                        , HalfCarry => True
                        ]
                    |> runTest
                )
            ]
        ]
