module Types exposing (..)


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


type Argument
    = Assumption Proposition
    | Argument Proposition (List Argument)



-- type OpenArgument
--     = OpenAssumption Proposition
--     | OpenArgument Proposition (List (List (Maybe OpenArgument)))
-- STRINGIFICATION


string =
    { fromArgument = fromArgument
    , fromFact = fromFact
    , fromProposition = fromProposition
    }


fromArgument : Argument -> String
fromArgument a =
    case a of
        Assumption p ->
            fromProposition p

        Argument p l ->
            "(["
                ++ (l
                        |> List.map fromArgument
                        |> List.sort
                        |> String.join ", "
                   )
                ++ "], "
                ++ fromProposition p
                ++ ")"


fromFact : Fact -> String
fromFact f =
    case f of
        Positive a ->
            a

        Negative a ->
            "¬" ++ a


fromProposition : Proposition -> String
fromProposition p =
    fromProposition_ True p


fromProposition_ : Bool -> Proposition -> String
fromProposition_ outer p =
    let
        ifInner a =
            if outer then
                ""

            else
                a

        join x a b =
            ifInner "("
                ++ String.join x (List.sort [ fromProposition a, fromProposition b ])
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
                ++ String.join " -> " [ fromProposition a, fromProposition b ]
                ++ ifInner ")"

        Equiv a b ->
            join " <-> " a b

        Not a ->
            "¬" ++ fromProposition a

        True_ ->
            "True"

        False_ ->
            "False"
