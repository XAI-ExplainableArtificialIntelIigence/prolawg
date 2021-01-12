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


type Support
    = Assumption Proposition
    | Support Argument


type alias TrackedFact =
    { support : List Support
    , conclusion : Fact
    }


type alias Argument =
    { support : List Support
    , conclusion : Fact
    }



-- STRINGIFICATION


string =
    { fromArgument = fromArgument
    , fromFact = fromFact
    , fromProposition = fromProposition
    , fromSupport = fromSupport
    , trackedFact = fromTrackedFact
    }


fromArgument : Argument -> String
fromArgument a =
    "(["
        ++ (a.support
                |> List.map fromSupport
                |> List.sort
                |> String.join ", "
           )
        ++ "], "
        ++ fromFact a.conclusion
        ++ ")"


fromTrackedFact : TrackedFact -> String
fromTrackedFact a =
    (a.support
        |> List.map fromSupport
        |> List.sort
        |> String.join ", "
    )
        ++ "; "
        ++ fromFact a.conclusion


fromSupport : Support -> String
fromSupport s =
    case s of
        Assumption p ->
            fromProposition p

        Support a_ ->
            fromArgument a_


fromFact : Fact -> String
fromFact f =
    case f of
        Positive a ->
            a

        Negative a ->
            "¬" ++ a



fromProposition : Proposition -> String
fromProposition p =
    let
        join x a b =
            "(" ++ String.join x (List.sort [ fromProposition a, fromProposition b ]) ++ ")"
    in
    case p of
        Variable a ->
            a

        And a b ->
            join " ∧ " a b

        Or a b ->
            join " ∨ " a b

        Implies a b ->
            join " -> " a b

        Equiv a b ->
            join " <-> " a b

        Not a ->
            "¬" ++ fromProposition a

        True_ ->
            "True"

        False_ ->
            "False"
