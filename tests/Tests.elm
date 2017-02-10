module Tests exposing (..)

import ByteTests
import Expect
import Test exposing (..)
import WordTests


all : Test
all =
    describe "EBoy"
        [ ByteTests.tests
        , WordTests.tests
        ]
