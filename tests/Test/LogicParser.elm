module Test.LogicParser exposing (..)

import Expect
import Logic exposing (..)
import LogicParser exposing (..)
import Parser
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    only <| describe "parser"
        [ test "1" <| \_ -> Expect.equal (parse "not ¬a and a <-> b or c /\\ !~d") (Nothing)
        ]
