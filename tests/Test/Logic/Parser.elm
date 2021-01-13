module Test.Logic.Parser exposing (..)

import Expect
import Logic.Core exposing (..)
import Logic.Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parser"
        [ test "1" <|
            \_ ->
                Expect.equal (parse "not ¬a and a <-> b or c /\\ !~d")
                    (Just
                        (Equiv
                            (And
                                (Not
                                    (Not
                                        (Variable "a")
                                    )
                                )
                                (Variable "a")
                            )
                            (Or
                                (Variable "b")
                                (And
                                    (Variable "c")
                                    (Not
                                        (Not
                                            (Variable "d")
                                        )
                                    )
                                )
                            )
                        )
                    )
        ]
