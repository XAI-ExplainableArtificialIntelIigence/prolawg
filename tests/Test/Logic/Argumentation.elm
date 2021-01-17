module Test.Logic.Argumentation exposing (..)

import Expect
import Logic.Argumentation exposing (..)
import Logic.Core exposing (..)
import Logic.Parser exposing (..)
import Maybe.Extra as Maybe
import Test exposing (..)



-- STRINGIFICATION HELPERS


explanation_ q l =
    Maybe.map2 explanation
        (parse q)
        (l
            |> List.map parseRanked
            |> Maybe.combine
            |> Maybe.map rankingToPreference
        )
        |> Maybe.map winnersLosersToStringlike


type alias Support_ =
    { pro : List Argument_
    , contra : List Argument_
    }


supportToStringlike { pro, contra } =
    { pro = List.map argumentToStringlike pro
    , contra = List.map argumentToStringlike contra
    }


type Argument_
    = Assumption_ String
    | Argument_ String Support_
    | Open_ (List String)


winnersLosersToStringlike { winners, losers } =
    { winners = supportToStringlike winners
    , losers = supportToStringlike losers
    }


argumentToStringlike a =
    case a of
        Assumption p ->
            Assumption_ (propositionToString p)

        Argument p s ->
            Argument_ (propositionToString p) (supportToStringlike s)

        Open fs ->
            Open_ (List.map factToString fs)



-- TESTS


suite : Test
suite =
    describe "elm-prolawg"
        [ describe "explanation"
            [ test "A?, A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A"
                            [ "A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [Assumption_ "A"] } })

            , test "A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A"
                            [ "not A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [Assumption_ "¬A"], pro = [] } })
            , test "¬A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "¬A"
                            [ "¬A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [Assumption_ "¬A"] } })
            , test "True?" <|
                \_ ->
                    Expect.equal
                        (explanation_ "True"
                            []
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [] } })
            , test "False?" <|
                \_ ->
                    Expect.equal
                        (explanation_ "False"
                            []
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [] } })
            , test "Complex tautology?" <|
                \_ ->
                    Expect.equal
                        (explanation_ "p or (q and r) -> (p or q) and (p or r)"
                            []
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [] } })
            , test "B?, A -> B, A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "B"
                            [ "A -> B"
                            , "A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [Argument_ "A -> B" { contra = [Open_ ["¬A"]], pro = [Assumption_ "A"] }] } })
            , test "C?, A, A -> B, B -> C" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A"
                            , "A -> B"
                            , "B -> C"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [Argument_ "B -> C" { contra = [Open_ ["¬B"]], pro = [Argument_ "A -> B" { contra = [Open_ ["¬A"]], pro = [Assumption_ "A"] }] }] } })
            , test "E?, A, A -> B, B -> C, C -> D, D -> E" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A"
                            , "A -> B"
                            , "B -> C"
                            , "C -> D"
                            , "D -> E"
                            ]
                        )
                        Nothing
            , test "C?, A and B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A and B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        Nothing
            , test "C?, A or B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A or B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        Nothing
            , test "A and B?, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A and B"
                            [ "A"
                            , "B"
                            ]
                        )
                        Nothing
            , test "A or B? A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A or B"
                            [ "A"
                            , "B"
                            ]
                        )
                        Nothing
            ]

        -- , describe "real world explanations"
        --     (let
        --         employmentLaw =
        --             [ Variable "employed"
        --             , Not (Variable "< 10 employees")
        --             , Not (Variable "reached old age insurance")
        --             , Variable "military official"
        --             , Variable "worked for >= 26 weeks"
        --             , Implies (Variable "employed") (Variable "can make request for change")
        --             , Implies (And (Variable "employed") (Variable "less than ten employees")) (Not (Variable "can make request for change"))
        --             , Implies (And (Variable "employed") (Variable "reached old age insurance")) (Not (Variable "can make request for change"))
        --             , Implies (And (Variable "employed") (Variable "worked for >= 26 weeks")) (Not (Variable "can make request for change"))
        --             ]
        --      in
        --      [ test "Roos (2016), p. 7-9" <|
        --         \_ ->
        --             Expect.equal
        --                 (explanation (Variable "s")
        --                     [ Or (Variable "p") (Variable "q")
        --                     , Not (Variable "q")
        --                     , Implies (Variable "p") (Variable "r")
        --                     , Implies (Variable "r") (Variable "s")
        --                     ]
        --                 )
        --                 [ [ { conclusion = Positive "s"
        --                     , support =
        --                         [ Assumption (Implies (Variable "r") (Variable "s"))
        --                         , Support
        --                             { conclusion = Positive "r"
        --                             , support =
        --                                 [ Assumption (Implies (Variable "p") (Variable "r"))
        --                                 , Support
        --                                     { conclusion = Positive "p"
        --                                     , support =
        --                                         [ Assumption (Or (Variable "p") (Variable "q"))
        --                                         , Support
        --                                             { conclusion = Negative "q"
        --                                             , support = [ Assumption (Not (Variable "q")) ]
        --                                             }
        --                                         ]
        --                                     }
        --                                 ]
        --                             }
        --                         ]
        --                     }
        --                   ]
        --                 ]
        --      , test "Mail by Nico" <|
        --         \_ ->
        --             Expect.equal
        --                 (explanation (Variable "q")
        --                     [ Variable "p"
        --                     , Implies (Variable "p") (Variable "q")
        --                     , Implies (Variable "q") (Variable "r")
        --                     , Implies (Variable "r") (Variable "¬q")
        --                     ]
        --                 )
        --                 [ [ { conclusion = Positive "q"
        --                     , support =
        --                         [ Assumption (Implies (Variable "p") (Variable "q"))
        --                         , Support
        --                             { conclusion = Positive "p"
        --                             , support = [ Assumption (Variable "p") ]
        --                             }
        --                         ]
        --                     }
        --                   ]
        --                 ]
        --      , test "Law example 1, pro" <|
        --         \_ ->
        --             Expect.equal
        --                 (explanation (Variable "can make request for change") employmentLaw)
        --                 Nothing
        --      , skip <|
        --         test "Law example 1, contra" <|
        --             \_ ->
        --                 Expect.equal
        --                     (explanation (Not (Variable "can make request for change")) employmentLaw)
        --                     Nothing
        --      ]
        --     )
        ]
