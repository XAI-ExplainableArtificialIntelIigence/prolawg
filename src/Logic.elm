module Logic exposing (..)

import Dict exposing (Dict)
import Html exposing (i)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe


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


equal : Fact -> Fact -> Bool
equal a b =
    case ( a, b ) of
        ( Positive a_, Positive b_ ) ->
            a_ == b_

        ( Negative a_, Negative b_ ) ->
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


cases : Proposition -> List (List Fact)
cases p =
    List.filter consistent (decompose p)


closures : Proposition -> List (List Fact)
closures p =
    List.filter inconsistent (decompose p)


reduce : List Proposition -> Proposition
reduce =
    List.foldl And True_


supports : List Proposition -> Proposition -> Bool
supports l a =
    cases (reduce (Not a :: l)) == []


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


factToComparable : Fact -> ( String, Int )
factToComparable a =
    case a of
        Positive b ->
            ( b, 1 )

        Negative b ->
            ( b, 0 )


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
        |> List.map (List.uniqueBy factToComparable)


questions : List (List Fact) -> List String
questions branches =
    shortestLength (paths branches)
        |> Maybe.map (\m -> List.filter (\facts -> List.length facts == m) (paths branches))
        |> Maybe.withDefault []
        |> List.concat
        |> List.map variable


type Support
    = Empty
    | Simple Proposition
    | Complex Support Support


type alias Argument =
    { support : Support
    , conclusion : Fact
    }


node : List Argument -> List Proposition -> List (List Argument)
node arguments rules =
    let
        _ =
            List.map (\a -> Debug.log "arg" a) arguments
    in
    rules
        |> List.select
        |> List.map
            (\( rule, rest ) ->
                arguments
                    |> List.find (\a -> inconsistent (a.conclusion :: List.concat (cases rule)))
                    |> Maybe.map (\a -> ( rule, rest, a.support ))
            )
        |> Maybe.values
        |> List.head
        |> Maybe.map
            (\( rule, rest, support ) ->
                cases rule
                    |> List.map
                        (\case_ ->
                            node
                                (List.map
                                    (\fact ->
                                        { support = Complex support (Simple rule)
                                        , conclusion = fact
                                        }
                                    )
                                    case_
                                    ++ arguments
                                )
                                rest
                        )
                    |> List.cartesianProduct
                    |> List.map List.concat
            )
        |> Maybe.withDefault
            (arguments
                |> List.uniquePairs
                |> List.filter (\( a, b ) -> contradicts a.conclusion b.conclusion)
                |> List.map (\( a, b ) -> [ a, b ])
            )


explanation : String -> List Proposition -> List (List Argument)
explanation question information =
    node [ { support = Simple (Variable "stupid support"), conclusion = Negative question } ] information


equalProposition : Proposition -> Proposition -> Bool
equalProposition p q =
    case ( p, q ) of
        ( Variable a, Variable b ) ->
            a == b

        ( Not a, Not b ) ->
            equalProposition a b

        ( And a b, And c d ) ->
            (a == c && b == d) || (a == d) && (b == c)

        ( Or a b, Or c d ) ->
            (a == c && b == d) || (a == d) && (b == c)

        ( Implies a b, Implies c d ) ->
            (a == c && b == d) || (a == d) && (b == c)

        ( Equiv a b, Equiv c d ) ->
            (a == c && b == d) || (a == d) && (b == c)

        _ ->
            False
