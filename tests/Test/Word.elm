module Test.Word exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import Word


suite : Test
suite =
    describe "Word"
        [ fuzz int "should always be in the range 0 <= x < 65536" <|
            \fuzzInt ->
                (Word.toInt <| Word.fromInt fuzzInt)
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 65536
                        ]
        ]
