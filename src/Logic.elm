module Logic exposing (..)

import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Types exposing (..)


decompose : Proposition -> List (List Fact)
decompose p =
    case p of
        Variable a ->
            [ [ Positive a ] ]

        And a b ->
            List.map List.concat
                (List.cartesianProduct
                    [ decompose a
                    , decompose b
                    ]
                )

        Or a b ->
            List.concat [ decompose a, decompose b ]

        Implies a b ->
            decompose (Or (Not a) b)

        Equiv a b ->
            decompose (And (Implies a b) (Implies b a))

        True_ ->
            [ [] ]

        False_ ->
            [ [ Positive "x", Negative "x" ] ]

        Not (Variable a) ->
            [ [ Negative a ] ]

        Not (And a b) ->
            decompose (Or (Not a) (Not b))

        Not (Or a b) ->
            decompose (And (Not a) (Not b))

        Not (Implies a b) ->
            decompose (Implies (Not b) (Not a))

        Not (Equiv a b) ->
            decompose (Or (Not (Implies a b)) (Not (Implies b a)))

        Not (Not a) ->
            decompose a

        Not True_ ->
            decompose False_

        Not False_ ->
            decompose True_


contradicts : Fact -> Fact -> Bool
contradicts a b =
    case ( a, b ) of
        ( Positive a_, Negative b_ ) ->
            a_ == b_

        ( Negative a_, Positive b_ ) ->
            a_ == b_

        _ ->
            False


inconsistent : List Fact -> Bool
inconsistent =
    List.uniquePairs
        >> List.any (\( a, b ) -> contradicts a b)


impossible : List (List Fact) -> Bool
impossible =
    List.all inconsistent


add : Fact -> List (List Fact) -> List (List Fact)
add a =
    List.map ((::) a)


closes : Fact -> List (List Fact) -> Bool
closes a l =
    impossible (add a l) && not (impossible l)


shrinks : Fact -> List (List Fact) -> Bool
shrinks a l =
    List.length (List.filter inconsistent (add a l))
        > List.length (List.filter inconsistent l)


consistent : List Fact -> Bool
consistent =
    List.uniquePairs
        >> List.all (\( a, b ) -> not (contradicts a b))


cases : Proposition -> List (List Fact)
cases p =
    List.filter consistent (decompose p)


closures : Proposition -> List (List Fact)
closures p =
    List.filter inconsistent (decompose p)



-- DECISION SUPPORT


reduce : List Proposition -> Proposition
reduce =
    List.foldl And True_


matters : Proposition -> List Proposition -> Bool
matters a l =
    List.any
        (\aa ->
            List.any
                (\b -> contradicts aa b)
                (List.concat (cases (reduce l)))
        )
        (List.concat (decompose a))


unknown : List Proposition -> List Proposition -> List (List Fact)
unknown question information =
    case
        List.find (\p -> matters p question) information
    of
        Nothing ->
            cases (reduce question)

        Just a ->
            unknown
                (a :: question)
                (List.filter ((/=) a) information)


variable : Fact -> String
variable fact =
    case fact of
        Positive a ->
            a

        Negative a ->
            a


shortestLength : List (List a) -> Maybe Int
shortestLength l =
    List.minimum (List.map List.length l)


paths : List (List Fact) -> List (List Fact)
paths branches =
    branches
        |> List.cartesianProduct
        |> List.map (List.uniqueBy string.fromFact)


questions : List (List Fact) -> List String
questions branches =
    shortestLength (paths branches)
        |> Maybe.map (\m -> List.filter (\facts -> List.length facts == m) (paths branches))
        |> Maybe.withDefault []
        |> List.concat
        |> List.map variable



-- EXPLANATION


argumentsForFact : List Proposition -> Fact -> List Argument
argumentsForFact information fact =
    simpleArgumentsForFact fact information
        ++ complexArgumentsForFact fact information


negateFact : Fact -> Fact
negateFact fact =
    case fact of
        Positive a ->
            Negative a

        Negative a ->
            Positive a


simpleArgumentsForFact : Fact -> List Proposition -> List Argument
simpleArgumentsForFact fact =
    List.filter (\p -> impossible (add (negateFact fact) (cases p)))
        >> List.map (\p -> { conclusion = fact, support = [ Assumption p ] })


complexArgumentsForFact : Fact -> List Proposition -> List Argument
complexArgumentsForFact fact information =
    information
        |> List.filter (\p -> shrinks (negateFact fact) (cases p))
        |> List.map
            (\p ->
                argumentsForCases (List.remove p information) (List.filter consistent (add (negateFact fact) (cases p)))
                    |> List.map
                        (\args ->
                            if List.length args > 0 then
                                Just
                                    { conclusion = fact
                                    , support = Assumption p :: (args |> List.map Support)
                                    }

                            else
                                Nothing
                        )
                    |> Maybe.values
            )
        |> List.concat


negate : List (List Fact) -> List (List Fact)
negate l =
    l |> List.cartesianProduct |> List.map (List.map negateFact)


argumentsForCases : List Proposition -> List (List Fact) -> List (List Argument)
argumentsForCases information l =
    l
        |> List.cartesianProduct
        |> List.map (List.map negateFact)
        |> List.map (List.map (argumentsForFact information))
        |> List.map List.cartesianProduct
        |> List.map List.concat
        |> List.map (List.uniqueBy string.fromArgument)
        |> List.uniqueBy (List.map string.fromArgument)


explanation : Proposition -> List Proposition -> List (List Argument)
explanation question information =
    let
        _ =
            Debug.log "?" question
    in
    argumentsForCases information (cases (Not question))
        |> List.map (List.filter applicable)



-- type Support
--     = Assumption Proposition
--     | Support Argument
-- type alias Argument =
--     { support : List Support
--     , conclusion : Fact
--     }


applicable : Argument -> Bool
applicable a =
    case a.support of
        h :: t ->
            List.all
                (\s ->
                    case s of
                        Assumption _ ->
                            True

                        Support a_ ->
                            applicable a_
                )
                (h :: t)

        [] ->
            False
