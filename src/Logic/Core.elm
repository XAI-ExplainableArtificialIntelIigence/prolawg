module Logic.Core exposing (..)

import List.Extra as List



-- FACTS


type Fact
    = Positive String
    | Negative String


variable : Fact -> String
variable a =
    case a of
        Positive b ->
            b

        Negative b ->
            b


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


consistent : List Fact -> Bool
consistent =
    List.uniquePairs
        >> List.all (\( a, b ) -> not (contradicts a b))



-- DNF, CNF


{-| Disjunctive normal form.
-}
type alias DNF =
    List (List Fact)


{-| Conjunctive normal form.
-}
type alias CNF =
    List (List Fact)


{-| Convert CNF <-> DNF.
-}
transform : List (List Fact) -> List (List Fact)
transform =
    List.cartesianProduct >> List.filter consistent


negateFact : Fact -> Fact
negateFact fact =
    case fact of
        Positive a ->
            Negative a

        Negative a ->
            Positive a


negate_ : DNF -> DNF
negate_ =
    List.cartesianProduct
        >> List.filter consistent
        >> List.map (List.map negateFact)


impossible : DNF -> Bool
impossible =
    List.all inconsistent


combine : DNF -> DNF -> DNF
combine a b =
    List.cartesianProduct [ a, b ]
        |> List.map List.concat


equivalent : DNF -> DNF -> Bool
equivalent a b =
    impossible (combine a (negate_ b))


closes : DNF -> DNF -> Bool
closes a l =
    impossible (combine a l)
        && not (impossible l)


shrinks : DNF -> DNF -> Bool
shrinks a b =
    List.length (List.filter inconsistent (combine a b))
        > List.length (List.filter inconsistent b)


consistentCases : DNF -> DNF -> DNF
consistentCases a b =
    List.filter (\c -> List.all consistent (combine [ c ] a)) b



-- PROPOSITIONS


type Proposition
    = Variable String
    | Not Proposition
    | And Proposition Proposition
    | Or Proposition Proposition
    | Implies Proposition Proposition
    | Equiv Proposition Proposition
    | True_
    | False_


dnf : Proposition -> DNF
dnf p =
    case p of
        Variable a ->
            [ [ Positive a ] ]

        And a b ->
            List.map List.concat
                (List.cartesianProduct
                    [ dnf a
                    , dnf b
                    ]
                )

        Or a b ->
            List.concat [ dnf a, dnf b ]

        Implies a b ->
            dnf (Or (Not a) b)

        Equiv a b ->
            dnf (And (Implies a b) (Implies b a))

        True_ ->
            [ [] ]

        False_ ->
            []

        Not (Variable a) ->
            [ [ Negative a ] ]

        Not (And a b) ->
            dnf (Or (Not a) (Not b))

        Not (Or a b) ->
            dnf (And (Not a) (Not b))

        Not (Implies a b) ->
            dnf (Implies (Not b) (Not a))

        Not (Equiv a b) ->
            dnf (Or (Not (Implies a b)) (Not (Implies b a)))

        Not (Not a) ->
            dnf a

        Not True_ ->
            dnf False_

        Not False_ ->
            dnf True_


opposite : Proposition -> Proposition
opposite p =
    case p of
        Not q ->
            q

        _ ->
            Not p


cases : Proposition -> DNF
cases p =
    List.filter consistent (dnf p)


closures : Proposition -> DNF
closures p =
    List.filter inconsistent (dnf p)



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
                ++ String.join x (List.sort [ propositionToString_ False a, propositionToString_ False b ])
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
                ++ String.join " -> " [ propositionToString_ False a, propositionToString_ False b ]
                ++ ifInner ")"

        Equiv a b ->
            join " <-> " a b

        Not a ->
            "¬" ++ propositionToString a

        True_ ->
            "True"

        False_ ->
            "False"
