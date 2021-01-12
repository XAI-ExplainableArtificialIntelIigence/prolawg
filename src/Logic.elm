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


combine : List (List Fact) -> List (List Fact) -> List (List Fact)
combine a b =
    List.cartesianProduct [ a, b ] |> List.map List.concat


closes : List (List Fact) -> List (List Fact) -> Bool
closes a l =
    impossible (combine a l) && not (impossible l)


shrinks : List (List Fact) -> List (List Fact) -> Bool
shrinks a b =
    List.length (List.filter inconsistent (combine a b))
        > List.length (List.filter inconsistent b)


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


argument : Proposition -> List Proposition -> Maybe Argument
argument question information =
    let
        support =
            information
                |> List.filter (\p -> shrinks (cases (Not question)) (cases p))
                |> List.map
                    (\p ->
                        let
                            simple =
                                if closes (cases p) (cases (Not question)) then
                                    [ [ Assumption p ] ]

                                else
                                    []

                            complex =
                                let
                                    _ =
                                        Debug.log "p" p

                                    premises =
                                        cases p
                                            |> List.filter (\c -> not (impossible (combine [ c ] (cases (Not question)))))
                                            |> negate
                                            |> List.map
                                                (\c ->
                                                    c
                                                        |> List.map (\fact -> argument (factToProposition fact) (List.remove p information))
                                                        
                                                        |> Maybe.combine
                                                )
                                            |> Maybe.values
                                in
                                if List.length premises > 0 then
                                    [ [ Argument p premises ] ]

                                else
                                    []
                        in
                        simple ++ complex
                    )
                |> List.concat
    in
    if List.length support > 0 then
        Just (Argument question support)

    else
        Nothing


explanation : Proposition -> List Proposition -> Maybe Argument
explanation question information =
    let
        _ =
            Debug.log "?" question
    in
    argument question information