module Logic.Core exposing (..)

import List.Extra as List



type Proposition
    = Variable String
    | Not Proposition
    | And Proposition Proposition
    | Or Proposition Proposition
    | Implies Proposition Proposition
    | Equiv Proposition Proposition
    | True_
    | False_


type Fact
    = Positive String
    | Negative String


{-| Disjunctive normal form.
-}
type alias DNF =
    List (List Fact)


decompose : Proposition -> DNF
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
            List.concat
                [ decompose a
                , decompose b

                --, decompose (And a b)
                ]

        Implies a b ->
            decompose (Or (Not a) b)

        Equiv a b ->
            decompose (And (Implies a b) (Implies b a))

        True_ ->
            [ [] ]

        False_ ->
            []

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


opposite : Proposition -> Proposition
opposite p =
    case p of
        Not q ->
            q

        _ ->
            Not p


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


impossible : DNF -> Bool
impossible =
    List.all inconsistent


add : Fact -> DNF -> DNF
add a =
    List.map ((::) a)


combine : DNF -> DNF -> DNF
combine a b =
    List.cartesianProduct [ a, b ] |> List.map List.concat


closes : DNF -> DNF -> Bool
closes a l =
    impossible (combine a l) && not (impossible l)


shrinks : DNF -> DNF -> Bool
shrinks a b =
    List.length (List.filter inconsistent (combine a b))
        > List.length (List.filter inconsistent b)


consistent : List Fact -> Bool
consistent =
    List.uniquePairs
        >> List.all (\( a, b ) -> not (contradicts a b))


cases : Proposition -> DNF
cases p =
    List.filter consistent (decompose p)


closures : Proposition -> DNF
closures p =
    List.filter inconsistent (decompose p)




negateFact : Fact -> Fact
negateFact fact =
    case fact of
        Positive a ->
            Negative a

        Negative a ->
            Positive a


negate_ : DNF -> DNF
negate_ l =
    l |> List.cartesianProduct |> List.map (List.map negateFact)


consistentCases : DNF -> DNF -> DNF
consistentCases a b =
    List.filter (\c -> List.all consistent (combine [ c ] a)) b



-- STRINGIFICATION


factToString : Fact -> String
factToString f =
    case f of
        Positive a ->
            a

        Negative a ->
            "¬" ++ a


propositionToString : Proposition -> String
propositionToString p =
    propositionToString_ True p


propositionToString_ : Bool -> Proposition -> String
propositionToString_ outer p =
    let
        ifInner a =
            if outer then
                ""

            else
                a

        join x a b =
            ifInner "("
                ++ String.join x (List.sort [ propositionToString a, propositionToString b ])
                ++ ifInner ")"
    in
    case p of
        Variable a ->
            a

        And a b ->
            join " ∧ " a b

        Or a b ->
            join " ∨ " a b

        Implies a b ->
            ifInner "("
                ++ String.join " -> " [ propositionToString a, propositionToString b ]
                ++ ifInner ")"

        Equiv a b ->
            join " <-> " a b

        Not a ->
            "¬" ++ propositionToString a

        True_ ->
            "True"

        False_ ->
            "False"
