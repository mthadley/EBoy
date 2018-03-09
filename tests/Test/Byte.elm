module Test.Byte exposing (..)

import Byte
import Expect
import Fuzz exposing (int)
import Test exposing (..)


suite : Test
suite =
    describe "Byte"
        [ fuzz int "should always be in the range 0 <= x < 256" <|
            \fuzzInt ->
                (Byte.toInt <| Byte.fromInt fuzzInt)
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 256
                        ]
        ]
