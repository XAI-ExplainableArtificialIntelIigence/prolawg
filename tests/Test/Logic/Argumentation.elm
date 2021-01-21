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
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Argument_ "A -> B" { contra = [ Open_ [ "¬A" ] ], pro = [ Assumption_ "A" ] } ] } })
            , test "C?, A, A -> B, B -> C" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A"
                            , "A -> B"
                            , "B -> C"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Argument_ "B -> C" { contra = [ Open_ [ "¬B" ] ], pro = [ Argument_ "A -> B" { contra = [ Open_ [ "¬A" ] ], pro = [ Assumption_ "A" ] } ] } ] } })
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
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [ Argument_ "C -> D" { contra = [ Open_ [ "D" ] ], pro = [ Argument_ "D -> E" { contra = [ Open_ [ "E" ] ], pro = [ Open_ [ "¬E" ] ] } ] } ], pro = [ Argument_ "B -> C" { contra = [ Open_ [ "¬B" ] ], pro = [ Argument_ "A -> B" { contra = [ Open_ [ "¬A" ] ], pro = [ Assumption_ "A" ] } ] } ] } })
            , test "C?, A and B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A and B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Argument_ "(A ∧ B) -> C" { contra = [ Open_ [ "¬A" ], Open_ [ "¬B" ] ], pro = [ Argument_ "A" { contra = [ Open_ [ "¬B" ] ], pro = [ Assumption_ "B" ] }, Argument_ "B" { contra = [ Open_ [ "¬A" ] ], pro = [ Assumption_ "A" ] } ] } ] } })
            , test "C?, A or B -> C, A, B" <|
                \_ ->
                    Expect.equal
                        (explanation_ "C"
                            [ "A or B -> C"
                            , "A"
                            , "B"
                            ]
                        )
                        (Just { losers = { contra = [], pro = [] }, winners = { contra = [], pro = [ Argument_ "(A ∨ B) -> C" { contra = [ Open_ [ "¬A", "¬B" ] ], pro = [ Assumption_ "A", Assumption_ "B" ] } ] } })
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
        , describe "real world explanations"
            [ skip <|
                test "Main example from Cremers 2016" <|
                    \_ ->
                        Expect.equal
                            (explanation_ "LEGAL_RequestedChangeWorkingTimes"
                                [ "1: (EmployedAt) -> OPPOWER_MakeRequest"
                                , "2: (EmployedAt /\\ LessThanTenEmployees) -> not OPPOWER_MakeRequest"
                                , "3: (EmployedAt /\\ ReacedOldAgeInsuranceAge) -> not OPPOWER_MakeRequest"
                                , "4: (EmployedAt /\\ MilitaryOfficial) -> not OPPOWER_MakeRequest"
                                , "5: (EmployedAt /\\ UnforseenCircumstances) -> OPPOWER_MakeRequest"
                                , "6: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingHours) -> LEGAL_RequestedChangeWorkingHours"
                                , "7: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingHours /\\ not RequestSubmittedInWriting) -> not LEGAL_RequestedChangeWorkingHours"
                                , "8: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingHours /\\ not TimeSinceLastRequestMinOneYear) -> not LEGAL_RequestedChangeWorkingHours"
                                , "9: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingHours /\\ not WorkedForAtleastTwentysixWeeks) -> not LEGAL_RequestedChangeWorkingHours"
                                , "10: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingHours /\\ UnforseenCircumtances) -> LEGAL_RequestedChangeWorkingHours"
                                , "11: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingTimes) -> LEGAL_RequestedChangeWorkingTimes"
                                , "12: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingTimes /\\ not RequestSubmittedInWriting) -> not LEGAL_RequestedChangeWorkingTimes"
                                , "13: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingTimes /\\ not TimeSinceLastRequestMinOneYear) -> not LEGAL_RequestedChangeWorkingTimes"
                                , "14: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingTimes /\\ not WorkedForAtleastTwentysixWeeks) -> not LEGAL_RequestedChangeWorkingTimes"
                                , "15: (OPPOWER_MakeRequest /\\ DOES_RequestChangeWorkingTimes /\\ UnforeseenCircumstances) -> LEGAL_RequestedChangeWorkingTimes"
                                , "1000: EmployedAt"
                                , "1000: not LessThanTenEmployees"
                                , "1000: not ReachedOldAgeInsurance"
                                , "1000: not TimeSinceLastRequestMinOneYear"
                                , "1000: not MilitaryOfficial"
                                , "1000: RequestedSubmittedInWriting"
                                , "1000: WorkedForAtLeastTwentySixWeeks"
                                , "1000: not UnforeseenCircumstances"
                                , "1000: DOES_RequestChangeWorkingHours"
                                , "1000: DOES_RequestChangeWorkingTimes"
                                ]
                            )
                            Nothing
            , test "British Nationality Act" <|
                \_ ->
                    Expect.equal
                        (explanation_ "IsBritishCitizen"
                            [ "0: (BornInUK /\\ BornAfterCommencement /\\ ParentBritishCitizen) -> BritishCitizen"
                            , "1: (FoundAsNewbornInUK /\\ FoundAfterCommencement /\\ not KnownToBeBornInNotUK /\\ not ParentKnownNotBritish) -> BritishCitizen"
                            , "2: (BornInUK /\\ IsMinor /\\ EntitledToBeRegistered /\\ ApplicationToBeRegistered) -> BritishCitizen"
                            , "3: (ParentBritishCitizen) -> EntitledToBeRegistered"
                            , "4: (ParentSettledInUnitedKingdom) -> EntitledToBeRegistered"
                            , "5: (ParentInArmedForces) -> EntitledToBeRegistered"
                            , "6: (BornInUK /\\ AtLeastTenYearsOld /\\ ForFirstTenYearsAbsentDaysLessThanNinety) -> EntitledToBeRegistered"
                            , "7: (AdoptionAuthorizedByBritishCourt /\\ SectionFiveRequirementsMet) -> BritishCitizen"
                            , "8: (AdoptedUnderConventionAdoption /\\ SectionFiveRequirementsMet) -> BritishCitizen"
                            , "9: (AtLeastOneAdopterBritishCitizen) -> SectionFiveRequirementsMet"
                            , "10: (AllAdoptersHabituallyResidentInUK) -> SectionFiveRequirementsMet"
                            , "11: (BornOutsideUK /\\ ParentIsBritishCitizenNotByDescent) -> BritishCitizen"
                            , "12: (BornOutsideUK /\\ ParentIsBritishCitizen /\\ ServingOutsideOfUK) -> BritishCitizen"
                            , "100: ForFirstTenYearsAbsentDaysLessThanNinety"
                            , "100: ¬AdoptionAuthorizedByBritishCourt"
                            , "100: ¬AdoptedUnderConventionAdoption"
                            , "100: AtLeastTenYearsOld"
                            , "100: ApplicationToBeRegistered"
                            , "100: IsMinor"
                            , "100: ¬ParentKnownNotBritish"
                            , "100: ¬FoundAfterCommencement"
                            ]
                        )
                        Nothing
            ]

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
