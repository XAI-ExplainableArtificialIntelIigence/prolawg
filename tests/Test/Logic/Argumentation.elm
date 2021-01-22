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
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Assumption_ "A" ] } })
            , test "A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A"
                            [ "not A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [ Assumption_ "¬A" ], pro = [] } })
            , test "¬A?, ¬A" <|
                \_ ->
                    Expect.equal
                        (explanation_ "¬A"
                            [ "¬A"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Assumption_ "¬A" ] } })
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
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = []
                                , pro =
                                    [ Argument_ "A -> B"
                                        { contra = [ Open_ [ "¬A" ] ]
                                        , pro = [ Assumption_ "A" ]
                                        }
                                    ]
                                }
                            }
                        )
            , test "C?, A, A -> B, B -> C" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A"
                            , "A -> B"
                            , "B -> C"
                            ]
                        )
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = []
                                , pro =
                                    [ Argument_ "B -> C"
                                        { contra = [ Open_ [ "¬B" ] ]
                                        , pro =
                                            [ Argument_ "A -> B"
                                                { contra = [ Open_ [ "¬A" ] ]
                                                , pro = [ Assumption_ "A" ]
                                                }
                                            ]
                                        }
                                    ]
                                }
                            }
                        )
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
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = [ Argument_ "C -> D" { contra = [ Open_ [ "D" ] ], pro = [ Argument_ "D -> E" { contra = [ Open_ [ "E" ] ], pro = [ Open_ [ "¬E" ] ] } ] } ]
                                , pro =
                                    [ Argument_ "B -> C"
                                        { contra = [ Open_ [ "¬B" ] ]
                                        , pro =
                                            [ Argument_ "A -> B"
                                                { contra = [ Open_ [ "¬A" ] ]
                                                , pro = [ Assumption_ "A" ]
                                                }
                                            ]
                                        }
                                    ]
                                }
                            }
                        )
            , test "C?, A and B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A and B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = []
                                , pro =
                                    [ Argument_ "(A ∧ B) -> C"
                                        { contra = [ Open_ [ "¬A" ], Open_ [ "¬B" ] ]
                                        , pro =
                                            [ Argument_ "A"
                                                { contra = [ Open_ [ "¬B" ] ]
                                                , pro = [ Assumption_ "B" ]
                                                }
                                            , Argument_ "B"
                                                { contra = [ Open_ [ "¬A" ] ]
                                                , pro = [ Assumption_ "A" ]
                                                }
                                            ]
                                        }
                                    ]
                                }
                            }
                        )
            , test "C?, A or B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A or B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = []
                                , pro =
                                    [ Argument_ "(A ∨ B) -> C"
                                        { contra = [ Open_ [ "¬A", "¬B" ] ]
                                        , pro = [ Assumption_ "A", Assumption_ "B" ]
                                        }
                                    ]
                                }
                            }
                        )
            , test "A and B?, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A and B"
                            [ "A"
                            , "B"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [] } })
            , test "A or B? A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "A or B"
                            [ "A"
                            , "B"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Assumption_ "A", Assumption_ "B" ] } })
            ]
        , describe "complex examples"
            (let
                employmentLaw =
                    [ "10: employed"
                    , "10: not less_than_ten_employees"
                    , "10: not reached_old_age_insurance"
                    , "10: military_official"
                    , "10: worked_for_more_than_twenty_six_weeks"
                    , "1: employed -> can_make_request_for_change"
                    , "2: employed and less_than_ten_employees -> not can_make_request_for_change"
                    , "2: employed and reached_old_age_insurance -> not can_make_request_for_change"
                    , "2: employed and worked_for_more_than_twenty_six_eeks -> not can_make_request_for_change"
                    ]
             in
             [ test "Roos (2016), p. 7-9" <|
                \_ ->
                    Expect.equal
                        (explanation_ "s"
                            [ "p or q"
                            , "not q"
                            , "p -> r"
                            , "r -> s"
                            ]
                        )
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = []
                                , pro =
                                    [ Argument_ "r -> s"
                                        { contra = [ Open_ [ "¬r" ] ]
                                        , pro =
                                            [ Argument_ "p -> r"
                                                { contra = [ Open_ [ "¬p" ] ]
                                                , pro =
                                                    [ Argument_ "p ∨ q"
                                                        { contra = [ Open_ [ "q" ] ]
                                                        , pro = [ Assumption_ "¬q" ]
                                                        }
                                                    ]
                                                }
                                            ]
                                        }
                                    ]
                                }
                            }
                        )
             , test "Mail by Nico" <|
                \_ ->
                    Expect.equal
                        (explanation_ "q"
                            [ "p"
                            , "p -> q"
                            , "q -> r"
                            , "r -> not q"
                            ]
                        )
                        (Just
                            { losers = { contra = [], pro = [] }
                            , winners =
                                { contra = [ Argument_ "q -> r" { contra = [ Open_ [ "r" ] ], pro = [ Open_ [ "¬r" ] ] }, Argument_ "r -> ¬q" { contra = [ Open_ [ "¬r" ] ], pro = [ Open_ [ "r" ] ] } ]
                                , pro =
                                    [ Argument_ "p -> q"
                                        { contra = [ Open_ [ "¬p" ] ]
                                        , pro = [ Assumption_ "p" ]
                                        }
                                    ]
                                }
                            }
                        )

             --  , test "Law example 1, pro" <|
             --     \_ ->
             --         Expect.equal
             --             (explanation_ "can_make_request_for_change" employmentLaw)
             --             Nothing
             --  , test "Law example 1, contra" <|
             --     \_ ->
             --         Expect.equal
             --             (explanation_ "not can_make_request_for_change" employmentLaw)
             --             Nothing
             ]
            )
        ]
