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


negate : List (List Fact) -> List (List Fact)
negate l =
    l |> List.cartesianProduct |> List.map (List.map negateFact)

    factToProposition : Fact -> Proposition
factToProposition f =
    case f of
        Positive a ->
            Variable a

        Negative a ->
            Not (Variable a)

