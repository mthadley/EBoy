module Test.Byte exposing (..)

import Byte
import Expect
import Fuzz exposing (int)
import Test exposing (..)


suite : Test
suite =
    describe "Byte"
        [ describe "fromInt"
            [ fuzz int "should always be in the range 0 <= x < 256" <|
                \fuzzInt ->
                    (Byte.toInt <| Byte.fromInt fuzzInt)
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.lessThan 256
                            ]
            ]
        , describe "rotateRight"
            [ test "should rotate right" <|
                \_ ->
                    Byte.fromInt 0x01
                        |> Byte.rotateRight
                        |> Byte.toInt
                        |> Expect.equal 0x80
            ]
        , describe "zero"
            [ test "Should be zero" <|
                \_ -> Expect.equal (Byte.toInt Byte.zero) 0
            ]
        ]
