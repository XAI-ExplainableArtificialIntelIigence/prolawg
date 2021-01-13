module Test.Logic.Argumentation exposing (..)

import Expect
import Logic.Argumentation exposing (..)
import Logic.Core exposing (..)
import Logic.Parser exposing (parse)
import Maybe.Extra as Maybe
import Test exposing (..)


explanation_ q l =
    Maybe.map2 (explanation (\_ _ -> Nothing)) (parse q) (l |> List.map parse |> Maybe.combine)


-- suite : Test
-- suite =
--     describe "elm-prolawg"
--         [ describe "explanation"
--             [ test "A?, A" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation_ "A"
--                             [ "A"
--                             ]
--                         )
--                         Nothing
--             , test "A?, ¬A" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation_ "A"
--                             [ "not A"
--                             ]
--                         )
--                         []

--             -- [[]]
--             , test "¬A?, ¬A" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Not (Variable "A"))
--                             [ Not (Variable "A")
--                             ]
--                         )
--                         Nothing
--             , test "True?" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation True_
--                             []
--                         )
--                         Nothing
--             , test "False?" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation False_
--                             []
--                         )
--                         Nothing
--             , test "Complex tautology?" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation
--                             (Implies
--                                 (Or
--                                     (Variable "p")
--                                     (And (Variable "q") (Variable "r"))
--                                 )
--                                 (And
--                                     (Or (Variable "p") (Variable "q"))
--                                     (Or (Variable "p") (Variable "r"))
--                                 )
--                             )
--                             []
--                         )
--                         [ []
--                         ]
--             , test "B?, A -> B, A" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "B")
--                             [ Implies (Variable "A") (Variable "B")
--                             , Variable "A"
--                             ]
--                         )
--                         [ [ { conclusion = Positive "B"
--                             , support =
--                                 [ Assumption (Implies (Variable "A") (Variable "B"))
--                                 , Support
--                                     { conclusion = Positive "A"
--                                     , support = [ Assumption (Variable "A") ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--             , test "C?, A, A -> B, B -> C" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "C")
--                             [ Variable "A"
--                             , Implies (Variable "A") (Variable "B")
--                             , Implies (Variable "B") (Variable "C")
--                             ]
--                         )
--                         [ [ { conclusion = Positive "C"
--                             , support =
--                                 [ Assumption (Implies (Variable "B") (Variable "C"))
--                                 , Support
--                                     { conclusion = Positive "B"
--                                     , support =
--                                         [ Assumption (Implies (Variable "A") (Variable "B"))
--                                         , Support
--                                             { conclusion = Positive "A"
--                                             , support = [ Assumption (Variable "A") ]
--                                             }
--                                         ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--             , test "E?, A, A -> B, B -> C, C -> D, D -> E" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "E")
--                             [ Variable "A"
--                             , Implies (Variable "A") (Variable "B")
--                             , Implies (Variable "B") (Variable "C")
--                             , Implies (Variable "C") (Variable "D")
--                             , Implies (Variable "D") (Variable "E")
--                             ]
--                         )
--                         [ [ { conclusion = Positive "E"
--                             , support =
--                                 [ Assumption (Implies (Variable "D") (Variable "E"))
--                                 , Support
--                                     { conclusion = Positive "D"
--                                     , support =
--                                         [ Assumption (Implies (Variable "C") (Variable "D"))
--                                         , Support
--                                             { conclusion = Positive "C"
--                                             , support =
--                                                 [ Assumption (Implies (Variable "B") (Variable "C"))
--                                                 , Support
--                                                     { conclusion = Positive "B"
--                                                     , support =
--                                                         [ Assumption (Implies (Variable "A") (Variable "B"))
--                                                         , Support
--                                                             { conclusion = Positive "A"
--                                                             , support = [ Assumption (Variable "A") ]
--                                                             }
--                                                         ]
--                                                     }
--                                                 ]
--                                             }
--                                         ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--             , test "C?, A and B -> C, A, B" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "C")
--                             [ Implies (And (Variable "A") (Variable "B")) (Variable "C")
--                             , Variable "A"
--                             , Variable "B"
--                             ]
--                         )
--                         [ [ { conclusion = Positive "C"
--                             , support =
--                                 [ Assumption (Implies (And (Variable "A") (Variable "B")) (Variable "C"))
--                                 , Support
--                                     { conclusion = Positive "A"
--                                     , support = [ Assumption (Variable "A") ]
--                                     }
--                                 , Support
--                                     { conclusion = Positive "B"
--                                     , support = [ Assumption (Variable "B") ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--             , test "C?, A or B -> C, A, B" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "C")
--                             [ Implies (Or (Variable "A") (Variable "B")) (Variable "C")
--                             , Variable "A"
--                             , Variable "B"
--                             ]
--                         )
--                         [ [ { conclusion = Positive "C"
--                             , support =
--                                 [ Assumption (Implies (Or (Variable "A") (Variable "B")) (Variable "C"))
--                                 , Support
--                                     { conclusion = Positive "A"
--                                     , support = [ Assumption (Variable "A") ]
--                                     }
--                                 ]
--                             }
--                           , { conclusion = Positive "C"
--                             , support =
--                                 [ Assumption (Implies (Or (Variable "A") (Variable "B")) (Variable "C"))
--                                 , Support
--                                     { conclusion = Positive "B"
--                                     , support = [ Assumption (Variable "B") ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--             , test "A and B?, A, B" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (And (Variable "A") (Variable "B"))
--                             [ Variable "A"
--                             , Variable "B"
--                             ]
--                         )
--                         [ [ { conclusion = Positive "A"
--                             , support = [ Assumption (Variable "A") ]
--                             }
--                           , { conclusion = Positive "B"
--                             , support = [ Assumption (Variable "B") ]
--                             }
--                           ]
--                         ]
--             , test "A or B? A, B" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Or (Variable "A") (Variable "B"))
--                             [ Variable "A"
--                             , Variable "B"
--                             ]
--                         )
--                         [ [ { conclusion = Positive "A"
--                             , support = [ Assumption (Variable "A") ]
--                             }
--                           ]
--                         , [ { conclusion = Positive "B", support = [ Assumption (Variable "B") ] } ]
--                         ]
--             ]
--         , describe "real world explanations"
--             (let
--                 employmentLaw =
--                     [ Variable "employed"
--                     , Not (Variable "< 10 employees")
--                     , Not (Variable "reached old age insurance")
--                     , Variable "military official"
--                     , Variable "worked for >= 26 weeks"
--                     , Implies (Variable "employed") (Variable "can make request for change")
--                     , Implies (And (Variable "employed") (Variable "less than ten employees")) (Not (Variable "can make request for change"))
--                     , Implies (And (Variable "employed") (Variable "reached old age insurance")) (Not (Variable "can make request for change"))
--                     , Implies (And (Variable "employed") (Variable "worked for >= 26 weeks")) (Not (Variable "can make request for change"))
--                     ]
--              in
--              [ test "Roos (2016), p. 7-9" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "s")
--                             [ Or (Variable "p") (Variable "q")
--                             , Not (Variable "q")
--                             , Implies (Variable "p") (Variable "r")
--                             , Implies (Variable "r") (Variable "s")
--                             ]
--                         )
--                         [ [ { conclusion = Positive "s"
--                             , support =
--                                 [ Assumption (Implies (Variable "r") (Variable "s"))
--                                 , Support
--                                     { conclusion = Positive "r"
--                                     , support =
--                                         [ Assumption (Implies (Variable "p") (Variable "r"))
--                                         , Support
--                                             { conclusion = Positive "p"
--                                             , support =
--                                                 [ Assumption (Or (Variable "p") (Variable "q"))
--                                                 , Support
--                                                     { conclusion = Negative "q"
--                                                     , support = [ Assumption (Not (Variable "q")) ]
--                                                     }
--                                                 ]
--                                             }
--                                         ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--              , test "Mail by Nico" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "q")
--                             [ Variable "p"
--                             , Implies (Variable "p") (Variable "q")
--                             , Implies (Variable "q") (Variable "r")
--                             , Implies (Variable "r") (Variable "¬q")
--                             ]
--                         )
--                         [ [ { conclusion = Positive "q"
--                             , support =
--                                 [ Assumption (Implies (Variable "p") (Variable "q"))
--                                 , Support
--                                     { conclusion = Positive "p"
--                                     , support = [ Assumption (Variable "p") ]
--                                     }
--                                 ]
--                             }
--                           ]
--                         ]
--              , test "Law example 1, pro" <|
--                 \_ ->
--                     Expect.equal
--                         (explanation (Variable "can make request for change") employmentLaw)
--                         Nothing
--              , skip <|
--                 test "Law example 1, contra" <|
--                     \_ ->
--                         Expect.equal
--                             (explanation (Not (Variable "can make request for change")) employmentLaw)
--                             Nothing
--              ]
--             )
--         ]
