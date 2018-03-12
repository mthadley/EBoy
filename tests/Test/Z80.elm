module Test.Z80 exposing (..)

import Basics.Extra exposing ((=>))
import Test exposing (Test, describe, test)
import Test.Util exposing (..)
import Z80.Flag exposing (Flag(..))
import Z80.Mode as Mode
import Z80.Registers exposing (..)


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
    , withCode [ 0x01, 0x01, 0x02 ]
        |> expectByte B 0x02
        |> expectByte C 0x01
        |> expectWord BC 0x0201
    , withCode [ 0x02 ]
        |> withByte A 0x23
        |> withWord BC 0x10
        |> expectMem 0x10 0x23
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
    , withCode [ 0x08, 0x22, 0x44 ]
        |> withWord SP 0x8888
        |> expectWord PC 0x03
        |> expectMemWord 0x4422 0x8888
    , withCode [ 0x09 ]
        |> withWord HL 0x0202
        |> expectWord BC 0x0202
    , withCode [ 0x0A ]
        |> withWord BC 0x08
        |> withMem 0x08 0x32
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
        |> withWord DE 0x1234
        |> withByte A 0x23
        |> expectMem 0x1234 0x23
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
                        ]
                    |> runTest
                )
            , test "Should set zero flag if result is zero"
                (withCode [ 0x0F ]
                    |> withByte A 0x00
                    |> expectByte A 0x00
                    |> expectFlags
                        [ Carry => False
                        , Zero => True
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
        ]
