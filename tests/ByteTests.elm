module ByteTests exposing (tests)

import Byte
import Test exposing (..)
import Expect
import Fuzz exposing (int)


tests : Test
tests =
    describe "Byte"
        [ fuzz int "should always be in the range 0 <= x < 256" <|
            \fuzzInt ->
                (Byte.toInt <| Byte.fromInt fuzzInt)
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 256
                        ]
        ]