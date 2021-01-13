module Test.Logic.Core exposing (..)

import Expect
import Logic.Core exposing (..)
import Logic.Parser exposing (parse)
import Test exposing (..)


reduce =
    List.foldl And True_


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
                        []
            , test "or true" <|
                \_ ->
                    Expect.equal
                        (decompose (Or (Variable "A") True_))
                        [ [ Positive "A" ], [] ]
            , test "or false" <|
                \_ ->
                    Expect.equal
                        (decompose (Or (Variable "A") False_))
                        [ [ Positive "A" ] ]
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
        ]
