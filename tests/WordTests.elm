module WordTests exposing (tests)

import Word
import Test exposing (..)
import Expect
import Fuzz exposing (int)


tests : Test
tests =
    describe "Word"
        [ fuzz int "should always be in the range 0 <= x < 65536" <|
            \fuzzInt ->
                (Word.toInt <| Word.fromInt fuzzInt)
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 65536
                        ]
        ]
