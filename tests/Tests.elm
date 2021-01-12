module Tests exposing (..)

import Expect
import Logic exposing (..)
import Test exposing (..)
import Types exposing (..)


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
        , describe "arguments for fact"
            [ test "A?, A" <|
                \_ ->
                    Expect.equal
                        (argumentsForFact
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            , Variable "B"
                            ]
                            (Positive "A")
                        )
                        [ { conclusion = Positive "A"
                          , support = [ Assumption (Variable "A") ]
                          }
                        ]
            , test "B?, A -> B, A" <|
                \_ ->
                    Expect.equal
                        (argumentsForFact
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            ]
                            (Positive "B")
                        )
                        [ { conclusion = Positive "B"
                          , support =
                                [ Assumption (Implies (Variable "A") (Variable "B"))
                                , Support
                                    { conclusion = Positive "A"
                                    , support = [ Assumption (Variable "A") ]
                                    }
                                ]
                          }
                        ]
            ]
        , describe "explanation"
            [ test "A?, A" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "A")
                            [ Variable "A"
                            ]
                        )
                        [ [ { conclusion = Positive "A"
                            , support = [ Assumption (Variable "A") ]
                            }
                          ]
                        ]
            , test "A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "A")
                            [ Not (Variable "A")
                            ]
                        )
                        [ [] ]
            , test "¬A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation (Not (Variable "A"))
                            [ Not (Variable "A")
                            ]
                        )
                        [ [ { conclusion = Negative "A"
                            , support = [ Assumption (Not (Variable "A")) ]
                            }
                          ]
                        ]
            , test "True?" <|
                \_ ->
                    Expect.equal
                        (explanation True_
                            []
                        )
                        [ []
                        ]
            , test "False?" <|
                \_ ->
                    Expect.equal
                        (explanation False_
                            []
                        )
                        []
            , test "Complex tautology?" <|
                \_ ->
                    Expect.equal
                        (explanation
                            (Implies
                                (Or
                                    (Variable "p")
                                    (And (Variable "q") (Variable "r"))
                                )
                                (And
                                    (Or (Variable "p") (Variable "q"))
                                    (Or (Variable "p") (Variable "r"))
                                )
                            )
                            []
                        )
                        [ []
                        ]
            , test "B?, A -> B, A" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "B")
                            [ Implies (Variable "A") (Variable "B")
                            , Variable "A"
                            ]
                        )
                        [ [ { conclusion = Positive "B"
                            , support =
                                [ Assumption (Implies (Variable "A") (Variable "B"))
                                , Support
                                    { conclusion = Positive "A"
                                    , support = [ Assumption (Variable "A") ]
                                    }
                                ]
                            }
                          ]
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
                        [ [ { conclusion = Positive "C"
                            , support =
                                [ Assumption (Implies (Variable "B") (Variable "C"))
                                , Support
                                    { conclusion = Positive "B"
                                    , support =
                                        [ Assumption (Implies (Variable "A") (Variable "B"))
                                        , Support
                                            { conclusion = Positive "A"
                                            , support = [ Assumption (Variable "A") ]
                                            }
                                        ]
                                    }
                                ]
                            }
                          ]
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
                        [ [ { conclusion = Positive "E"
                            , support =
                                [ Assumption (Implies (Variable "D") (Variable "E"))
                                , Support
                                    { conclusion = Positive "D"
                                    , support =
                                        [ Assumption (Implies (Variable "C") (Variable "D"))
                                        , Support
                                            { conclusion = Positive "C"
                                            , support =
                                                [ Assumption (Implies (Variable "B") (Variable "C"))
                                                , Support
                                                    { conclusion = Positive "B"
                                                    , support =
                                                        [ Assumption (Implies (Variable "A") (Variable "B"))
                                                        , Support
                                                            { conclusion = Positive "A"
                                                            , support = [ Assumption (Variable "A") ]
                                                            }
                                                        ]
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                          ]
                        ]
            , test "C?, A and B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "C")
                            [ Implies (And (Variable "A") (Variable "B")) (Variable "C")
                            , Variable "A"
                            , Variable "B"
                            ]
                        )
                        [ [ { conclusion = Positive "C"
                            , support =
                                [ Assumption (Implies (And (Variable "A") (Variable "B")) (Variable "C"))
                                , Support
                                    { conclusion = Positive "A"
                                    , support = [ Assumption (Variable "A") ]
                                    }
                                , Support
                                    { conclusion = Positive "B"
                                    , support = [ Assumption (Variable "B") ]
                                    }
                                ]
                            }
                          ]
                        ]
            , test "C?, A or B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "C")
                            [ Implies (Or (Variable "A") (Variable "B")) (Variable "C")
                            , Variable "A"
                            , Variable "B"
                            ]
                        )
                        [ [ { conclusion = Positive "C"
                            , support =
                                [ Assumption (Implies (Or (Variable "A") (Variable "B")) (Variable "C"))
                                , Support
                                    { conclusion = Positive "A"
                                    , support = [ Assumption (Variable "A") ]
                                    }
                                ]
                            }
                          , { conclusion = Positive "C"
                            , support =
                                [ Assumption (Implies (Or (Variable "A") (Variable "B")) (Variable "C"))
                                , Support
                                    { conclusion = Positive "B"
                                    , support = [ Assumption (Variable "B") ]
                                    }
                                ]
                            }
                          ]
                        ]
            , test "A and B?, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation (And (Variable "A") (Variable "B"))
                            [ Variable "A"
                            , Variable "B"
                            ]
                        )
                        [ [ { conclusion = Positive "A"
                            , support = [ Assumption (Variable "A") ]
                            }
                          , { conclusion = Positive "B"
                            , support = [ Assumption (Variable "B") ]
                            }
                          ]
                        ]
            , test "A or B? A, B" <|
                \_ ->
                    Expect.equal
                        (explanation (Or (Variable "A") (Variable "B"))
                            [ Variable "A"
                            , Variable "B"
                            ]
                        )
                        [ [ { conclusion = Positive "A"
                            , support = [ Assumption (Variable "A") ]
                            }
                          ]
                        , [ { conclusion = Positive "B", support = [ Assumption (Variable "B") ] } ]
                        ]
            ]
        , describe "real world explanations"
            (let
                employmentLaw =
                    [ Variable "employed"
                    , Not (Variable "< 10 employees")
                    , Not (Variable "reached old age insurance")
                    , Variable "military official"
                    , Variable "worked for >= 26 weeks"
                    , Implies (Variable "employed") (Variable "can make request for change")
                    , Implies (And (Variable "employed") (Variable "less than ten employees")) (Not (Variable "can make request for change"))
                    , Implies (And (Variable "employed") (Variable "reached old age insurance")) (Not (Variable "can make request for change"))
                    , Implies (And (Variable "employed") (Variable "worked for >= 26 weeks")) (Not (Variable "can make request for change"))
                    ]
             in
             [ test "Roos (2016), p. 7-9" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "s")
                            [ Or (Variable "p") (Variable "q")
                            , Not (Variable "q")
                            , Implies (Variable "p") (Variable "r")
                            , Implies (Variable "r") (Variable "s")
                            ]
                        )
                        [ [ { conclusion = Positive "s"
                            , support =
                                [ Assumption (Implies (Variable "r") (Variable "s"))
                                , Support
                                    { conclusion = Positive "r"
                                    , support =
                                        [ Assumption (Implies (Variable "p") (Variable "r"))
                                        , Support
                                            { conclusion = Positive "p"
                                            , support =
                                                [ Assumption (Or (Variable "p") (Variable "q"))
                                                , Support
                                                    { conclusion = Negative "q"
                                                    , support = [ Assumption (Not (Variable "q")) ]
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                          ]
                        ]
             , test "Mail by Nico" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "q")
                            [ Variable "p"
                            , Implies (Variable "p") (Variable "q")
                            , Implies (Variable "q") (Variable "r")
                            , Implies (Variable "r") (Variable "¬q")
                            ]
                        )
                        [ [ { conclusion = Positive "q"
                            , support =
                                [ Assumption (Implies (Variable "p") (Variable "q"))
                                , Support
                                    { conclusion = Positive "p"
                                    , support = [ Assumption (Variable "p") ]
                                    }
                                ]
                            }
                          ]
                        ]
             , test "Law example 1, pro" <|
                \_ ->
                    Expect.equal
                        (explanation (Variable "can make request for change") employmentLaw)
                        [ [ { conclusion = Positive "can make request for change"
                            , support =
                                [ Assumption (Implies (Variable "employed") (Variable "can make request for change"))
                                , Support
                                    { conclusion = Positive "employed"
                                    , support = [ Assumption (Variable "employed") ]
                                    }
                                ]
                            }
                          ]
                        ]
                , skip <| test "Law example 1, contra" <|
                \_ ->
                    Expect.equal
                        (explanation (Not (Variable "can make request for change")) employmentLaw)
                        [ [ { conclusion = Positive "can make request for change"
                            , support =
                                [ Assumption (Implies (Variable "employed") (Variable "can make request for change"))
                                , Support
                                    { conclusion = Positive "employed"
                                    , support = [ Assumption (Variable "employed") ]
                                    }
                                ]
                            }
                          ]
                        ]
             ]
            )
        ]
