module Test.Word exposing (suite)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import Word


suite : Test
suite =
    describe "Word"
        [ describe "fromInt"
            [ fuzz int "should always be in the range 0 <= x < 65536" <|
                \fuzzInt ->
                    (Word.toInt <| Word.fromInt fuzzInt)
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.lessThan 65536
                            ]
            ]
        , describe "zero"
            [ test "Should be zero" <|
                \_ -> Expect.equal (Word.toInt Word.zero) 0
            ]
        ]
