module Test.Z80 exposing (..)

import Test exposing (..)
import Test.Util
    exposing
        ( Unit
        , expectByte
        , expectMem
        , expectWord
        , toTest
        , withByte
        , withCode
        , withWord
        )
import Z80.Registers exposing (..)


states : List Unit
states =
    [ withCode [ 0x00 ]
        |> expectByte A 0
        |> expectByte B 0
        |> expectByte C 0
        |> expectByte D 0
        |> expectByte E 0
        |> expectByte H 0
        |> expectByte L 0
        |> expectByte F 0
        |> expectWord PC 1
        |> expectWord SP 0xFFFE
        |> expectWord BC 0
        |> expectWord HL 0
        |> expectWord DE 0
        |> expectWord AF 0
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
    ]


suite : Test
suite =
    states
        |> List.map toTest
        |> describe "Z80 op codes"
