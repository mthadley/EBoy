module Test.Z80 exposing (..)

import Basics.Extra exposing ((=>))
import Test exposing (..)
import Test.Util exposing (..)
import Z80.Flag exposing (Flag(..))
import Z80.Mode as Mode
import Z80.Registers exposing (..)


tests : List Unit
tests =
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
    , withCode [ 0x07, 0x23 ]
        |> withByte A 0x04
        |> expectByte A 0x08
    , withCode [ 0x08, 0x22, 0x44 ]
        |> withWord SP 0x8888
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
    ]


suite : Test
suite =
    tests
        |> List.map toTest
        |> describe "Z80 op codes"
