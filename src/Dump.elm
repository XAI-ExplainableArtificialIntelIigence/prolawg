module Dump exposing (..)

import Logic exposing (..)


equal : Fact -> Fact -> Bool
equal a b =
    case ( a, b ) of
        ( Positive a_, Positive b_ ) ->
            a_ == b_

        ( Negative a_, Negative b_ ) ->
            a_ == b_

        _ ->
            False

supports : List Proposition -> Proposition -> Bool
supports l a =
    cases (reduce (Not a :: l)) == []

decomposeArgument : Argument -> List (List TrackedFact)
decomposeArgument a =
    List.map
        (\case_ ->
            List.map
                (\fact ->
                    { support = a.support
                    , conclusion = fact
                    }
                )
                case_
        )
        (decompose a.conclusion)


decomposeArguments : List Argument -> List (List TrackedFact)
decomposeArguments l =
    List.map List.concat
        (List.cartesianProduct
            (List.map decomposeArgument l)
        )

tableau : List Argument -> List Proposition -> List (List TrackedFact)
tableau root information =
    case
        List.find (\p -> matters p (List.map .conclusion root)) information
    of
        Just a ->
            tableau
                ({ support = [ Simple a ], conclusion = a } :: root)
                (List.filter ((/=) a) information)

        Nothing ->
            decomposeArguments root




trackedFactsForClosure : List TrackedFact -> List (List TrackedFact)
trackedFactsForClosure l =
    l
        |> List.uniquePairs
        |> List.filter (\( a, b ) -> contradicts a.conclusion b.conclusion)
        |> List.map (\( a, b ) -> [ a, b ])


mergeTrackedFacts : List (List (List TrackedFact)) -> List (List TrackedFact)
mergeTrackedFacts branches =
    branches
        |> List.cartesianProduct
        |> List.map List.concat
        |> List.map (List.uniqueBy trackedFactToString)
        |> List.map (List.sortBy trackedFactToString)
        |> List.uniqueBy (List.map trackedFactToString)

explanation___ : Proposition -> List Proposition -> List (List TrackedFact)
explanation___ question information =
    let
        test =
            Simple (Not question)
    in
    tableau [ { support = [ test ], conclusion = Not question } ] information
        |> List.map trackedFactsForClosure
        |> mergeTrackedFacts
        |> List.filter (List.any (\a -> not (a.support == [ test ])))


negateFact : Fact -> Fact
negateFact fact =
    case fact of
        Positive a ->
            Negative a

        Negative a ->
            Positive a





factToProposition : Fact -> Proposition
factToProposition f =
    case f of
        Positive a ->
            Variable a

        Negative a ->
            Not (Variable a)


explanation : Proposition -> List Proposition -> Maybe (List (List Support))
explanation question information =
    let
        _ =
            Debug.log "?" question
    in
    explanation_ (decompose (Not question)) information


explanation_ : List (List Fact) -> List Proposition -> Maybe (List (List Support))
explanation_ question information =
    if List.filter consistent question == [] then
        Nothing

    else
        Just
            (question
                |> List.filter consistent
                |> List.map (supportForCase information)
                |> List.concat
            )


supportForCase : List Proposition -> List Fact -> List (List Support)
supportForCase information case_ =
    case_ |> List.map (supportForFact information)


supportForFact : List Proposition -> Fact -> List Support
supportForFact information fact =
    information
        |> List.select
        |> List.map
            (\( p, rest ) ->
                case explanation_ (List.map ((::) fact) (cases p)) rest of
                    Nothing ->
                        Simple p

                    Just l ->
                        Complex { support = l, conclusion = fact }
            )
complexArgumentsForFact : Fact -> List Proposition -> List Argument
complexArgumentsForFact fact =
    List.filter (\p -> shrinks (negateFact fact) (cases p))
        >> List.select
        >> List.map
            (\( p, rest ) ->
                List.filter consistent (add (negateFact fact) (cases p))
                    |> List.map
                        (List.map (argumentsForFact rest)
                            >> List.cartesianProduct
                            >> List.map
                                (\args ->
                                    { conclusion = fact
                                    , support = Simple p :: (args |> List.map Complex)
                                    }
                                )
                        )
                    |> List.concat
            )
        >> List.concat