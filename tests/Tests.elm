module Tests exposing (..)

import Expect
import Logic exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "elm-prolawg"
        [ describe "decompose"
            [ test "not" <|
                \_ ->
                    Expect.equal
                        (decompose (Not (Variable "A")))
                        [ [ Negative "A" ] ]
            , test "and" <|
                \_ ->
                    Expect.equal
                        (decompose (And (Variable "A") (Variable "B")))
                        [ [ Positive "A", Positive "B" ] ]
            , test "or" <|
                \_ ->
                    Expect.equal
                        (decompose (Or (Variable "A") (Variable "B")))
                        [ [ Positive "A" ], [ Positive "B" ] ]
            , test "implies" <|
                \_ ->
                    Expect.equal
                        (decompose (Implies (Variable "A") (Variable "B")))
                        [ [ Negative "A" ], [ Positive "B" ] ]
            , test "equiv" <|
                \_ ->
                    Expect.equal
                        (decompose (Equiv (Variable "A") (Variable "B")))
                        [ [ Negative "A", Negative "B" ]
                        , [ Negative "A", Positive "A" ]
                        , [ Positive "B", Negative "B" ]
                        , [ Positive "B", Positive "A" ]
                        ]
            , test "and true" <|
                \_ ->
                    Expect.equal
                        (decompose (And (Variable "A") True_))
                        [ [ Positive "A" ] ]
            , test "and false" <|
                \_ ->
                    Expect.equal
                        (decompose (And (Variable "A") False_))
                        [ [ Positive "A", Positive "x", Negative "x" ] ]
            , test "or true" <|
                \_ ->
                    Expect.equal
                        (decompose (Or (Variable "A") True_))
                        [ [ Positive "A" ], [] ]
            , test "or false" <|
                \_ ->
                    Expect.equal
                        (decompose (Or (Variable "A") False_))
                        [ [ Positive "A" ], [ Positive "x", Negative "x" ] ]
            ]
        , describe "cases"
            [ test "not" <|
                \_ ->
                    Expect.equal
                        (cases (Not (Variable "A")))
                        [ [ Negative "A" ] ]
            , test "and" <|
                \_ ->
                    Expect.equal
                        (cases (And (Variable "A") (Variable "B")))
                        [ [ Positive "A", Positive "B" ] ]
            , test "or" <|
                \_ ->
                    Expect.equal
                        (cases (Or (Variable "A") (Variable "B")))
                        [ [ Positive "A" ], [ Positive "B" ] ]
            , test "implies" <|
                \_ ->
                    Expect.equal
                        (cases (Implies (Variable "A") (Variable "B")))
                        [ [ Negative "A" ], [ Positive "B" ] ]
            , test "equiv" <|
                \_ ->
                    Expect.equal
                        (cases (Equiv (Variable "A") (Variable "B")))
                        [ [ Negative "A", Negative "B" ]
                        , [ Positive "B", Positive "A" ]
                        ]
            , test "and and" <|
                \_ ->
                    Expect.equal
                        (cases (And (And (Variable "A") (Variable "B")) (Variable "C")))
                        [ [ Positive "A", Positive "B", Positive "C" ] ]
            , test "and or" <|
                \_ ->
                    Expect.equal
                        (cases (Or (And (Variable "A") (Variable "B")) (Variable "C")))
                        [ [ Positive "A", Positive "B" ], [ Positive "C" ] ]
            , test "or or" <|
                \_ ->
                    Expect.equal
                        (cases (Or (Or (Variable "A") (Variable "B")) (Variable "C")))
                        [ [ Positive "A" ], [ Positive "B" ], [ Positive "C" ] ]
            , test "¬C, A or B -> C" <|
                \_ ->
                    Expect.equal
                        (cases <|
                            reduce
                                [ Not (Variable "C")
                                , Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                                ]
                        )
                        [ [ Negative "A", Negative "B", Negative "C" ] ]
            , test "¬C, A or B -> C, D -> F" <|
                \_ ->
                    Expect.equal
                        (cases <|
                            reduce
                                [ Not (Variable "C")
                                , Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                                , Implies (Variable "D") (Variable "F")
                                ]
                        )
                        [ [ Negative "D", Negative "A", Negative "B", Negative "C" ]
                        , [ Positive "F", Negative "A", Negative "B", Negative "C" ]
                        ]
            ]
        , describe "closures"
            [ test "¬C" <|
                \_ ->
                    Expect.equal
                        (closures <|
                            reduce
                                [ Not (Variable "C") ]
                        )
                        []
            , test "¬C, D -> F" <|
                \_ ->
                    Expect.equal
                        (closures <|
                            reduce
                                [ Not (Variable "C")
                                , Implies (Variable "D") (Variable "F")
                                ]
                        )
                        []
            , test "¬C, A or B -> C" <|
                \_ ->
                    Expect.equal
                        (closures <|
                            reduce
                                [ Not (Variable "C")
                                , Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                                ]
                        )
                        [ [ Positive "C", Negative "C" ] ]
            , test "¬C, A or B -> C, D -> F" <|
                \_ ->
                    Expect.equal
                        (closures <|
                            reduce
                                [ Not (Variable "C")
                                , Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                                , Implies (Variable "D") (Variable "F")
                                ]
                        )
                        [ [ Negative "D", Positive "C", Negative "C" ], [ Positive "F", Positive "C", Negative "C" ] ]
            ]
        , describe "matters"
            [ test "A -> B for ¬B" <|
                \_ ->
                    Expect.equal
                        (matters (Implies (Variable "A") (Variable "B"))
                            [ Not (Variable "B") ]
                        )
                        True
            , test "D -> F for ¬C" <|
                \_ ->
                    Expect.equal
                        (matters (Implies (Variable "D") (Variable "F"))
                            [ Not (Variable "C") ]
                        )
                        False
            , test "D -> F for ¬C, A or B -> C" <|
                \_ ->
                    Expect.equal
                        (matters (Implies (Variable "D") (Variable "F"))
                            [ Not (Variable "C")
                            , Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                            ]
                        )
                        False
            ]
        , describe "unknown"
            [ test "A?" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "A") ] [])
                        [ [ Negative "A" ] ]
            , test "B?, A -> B" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "B") ]
                            [ Implies (Variable "A") (Variable "B")
                            ]
                        )
                        [ [ Negative "B", Negative "A" ] ]
            , test "C?, A and B -> C" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "C") ]
                            [ Implies (And (Variable "A") (Variable "B")) (Variable "C")
                            ]
                        )
                        [ [ Negative "C", Negative "A" ], [ Negative "C", Negative "B" ] ]
            , test "C?, A or B -> C" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "C") ]
                            [ Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                            ]
                        )
                        [ [ Negative "C", Negative "A", Negative "B" ] ]
            , test "C?, A or B -> C, D -> F" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "C") ]
                            [ Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                            , Implies (Variable "D") (Variable "F")
                            ]
                        )
                        [ [ Negative "C", Negative "A", Negative "B" ] ]
            , test "E?, A and B -> E, C or D -> E, F and G, H or I" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "E") ]
                            [ Implies (And (Variable "A") (Variable "B")) (Variable "E")
                            , Implies (Or (Variable "C") (Variable "D")) (Variable "E")
                            , And (Variable "F") (Variable "G")
                            , Or (Variable "H") (Variable "I")
                            ]
                        )
                        [ [ Negative "E", Negative "A", Negative "C", Negative "D" ]
                        , [ Negative "E", Negative "B", Negative "C", Negative "D" ]
                        ]
            , test "B?, A -> B, A" <|
                \_ ->
                    Expect.equal
                        (unknown [ Not (Variable "B") ]
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            ]
                        )
                        []
            , test "C?, A or B -> C, E -> F, D, ¬D" <|
                \_ ->
                    Expect.equal
                        (unknown
                            [ Not (Variable "C")
                            , Variable "D"
                            , Not (Variable "D")
                            ]
                            [ Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                            , Implies (Variable "E") (Variable "F")
                            ]
                        )
                        []
            ]
        , describe "questions"
            [ test "EACD, EBCD" <|
                \_ ->
                    Expect.equal
                        (questions
                            [ [ Negative "E", Negative "A", Negative "C", Negative "D" ]
                            , [ Negative "E", Negative "B", Negative "C", Negative "D" ]
                            ]
                        )
                        [ "E", "C", "D" ]
            ]
        , describe "tableau"
            [ test "B?, A, A -> B" <|
                \_ ->
                    Expect.equal
                        (tableau
                            [ { support = [ Simple (Not (Variable "B")) ]
                              , conclusion = Not (Variable "B")
                              }
                            ]
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            ]
                        )
                        [ [ { conclusion = Positive "A"
                            , support = [ Simple (Variable "A") ]
                            }
                          , { conclusion = Negative "A"
                            , support = [ Simple (Implies (Variable "A") (Variable "B")) ]
                            }
                          , { conclusion = Negative "B"
                            , support = [ Simple (Not (Variable "B")) ]
                            }
                          ]
                        , [ { conclusion = Positive "A"
                            , support = [ Simple (Variable "A") ]
                            }
                          , { conclusion = Positive "B"
                            , support = [ Simple (Implies (Variable "A") (Variable "B")) ]
                            }
                          , { conclusion = Negative "B"
                            , support = [ Simple (Not (Variable "B")) ]
                            }
                          ]
                        ]
            ]
        , describe "argumentsForClosure"
            [ test "A, ¬A, ¬B" <|
                \_ ->
                    Expect.equal
                        (argumentsForClosure
                            [ { conclusion = Positive "A"
                              , support = [ Simple (Variable "A") ]
                              }
                            , { conclusion = Negative "A"
                              , support = [ Simple (Implies (Variable "A") (Variable "B")) ]
                              }
                            , { conclusion = Negative "B"
                              , support = [ Simple (Not (Variable "B")) ]
                              }
                            ]
                        )
                        [ [ Simple (Variable "A"), Simple (Implies (Variable "A") (Variable "B")) ] ]
            , test "A, B, ¬B" <|
                \_ ->
                    Expect.equal
                        (argumentsForClosure
                            [ { conclusion = Positive "A"
                              , support = [ Simple (Variable "A") ]
                              }
                            , { conclusion = Positive "B"
                              , support = [ Simple (Implies (Variable "A") (Variable "B")) ]
                              }
                            , { conclusion = Negative "B"
                              , support = [ Simple (Not (Variable "B")) ]
                              }
                            ]
                        )
                        [ [ Simple (Implies (Variable "A") (Variable "B")), Simple (Not (Variable "B")) ] ]
            ]
        , describe "explanation"
            [ test "A?, A" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "A")
                            [ Variable "A"
                            ]
                        )
                        [ [ Simple (Variable "A") ]
                        ]
            , test "B?, A -> B, A" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "B")
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            ]
                        )
                        [ [ Simple (Implies (Variable "A") (Variable "B")), Simple (Variable "A") ]
                        ]
            , test "C?, A, A -> B, B -> C" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "C")
                            [ Variable "A"
                            , Implies (Variable "A") (Variable "B")
                            , Implies (Variable "B") (Variable "C")
                            ]
                        )
                        [ [ Simple (Implies (Variable "A") (Variable "B")), Simple (Implies (Variable "B") (Variable "C")), Simple (Variable "A") ]
                        ]
            , test "E?, A, A -> B, B -> C, C -> D, D -> E" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "E")
                            [ Variable "A"
                            , Implies (Variable "A") (Variable "B")
                            , Implies (Variable "B") (Variable "C")
                            , Implies (Variable "C") (Variable "D")
                            , Implies (Variable "D") (Variable "E")
                            ]
                        )
                        [ [ Simple (Implies (Variable "A") (Variable "B")), Simple (Implies (Variable "B") (Variable "C")), Simple (Implies (Variable "C") (Variable "D")), Simple (Implies (Variable "D") (Variable "E")), Simple (Variable "A") ] ]
            ]
        ]
