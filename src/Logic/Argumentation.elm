module Logic.Argumentation exposing (..)

import List.Extra as List
import Logic.Core exposing (..)
import Maybe
import Maybe.Extra as Maybe


type alias Preference =
    Proposition -> Proposition -> Maybe Bool


type Argument
    = Assumption Proposition
    | Argument
        Proposition
        { pro : List Argument
        , contra : List Argument
        }
    | Open (List Fact)


head : Argument -> Maybe Proposition
head a =
    case a of
        Assumption h ->
            Just h

        Argument h _ ->
            Just h

        Open _ ->
            Nothing


type RelevantArgument
    = RelevantAssumption Proposition
    | RelevantArgument Proposition Support
    | RelevantOpen (List Fact)


type Defeated
    = DefeatedClosed Proposition
    | DefeatedOpen (List Fact)


type alias Support =
    { relevant :
        { pro : List RelevantArgument
        , contra : List RelevantArgument
        }
    , irrelevant :
        { pro : List Defeated
        , contra : List Defeated
        }
    }


{-| Performs resolution.
-}
arguments : DNF -> List Proposition -> List Argument
arguments question information =
    information
        |> List.map
            (\p ->
                let
                    cases_ =
                        cases p

                    negatedQuestion =
                        negate_ question

                    restQuestion =
                        consistentCases cases_ negatedQuestion ++ consistentCases negatedQuestion cases_

                    relevant =
                        List.length restQuestion < (List.length cases_ * List.length negatedQuestion)

                    decisive =
                        restQuestion == []
                in
                case ( relevant, decisive ) of
                    ( True, True ) ->
                        Just (Assumption p)

                    ( True, False ) ->
                        proContra (negate_ restQuestion) (List.remove p information)
                            |> Maybe.map (\l -> Argument p l)

                    ( False, _ ) ->
                        Nothing
            )
        |> Maybe.values
        |> (\l ->
                case l of
                    [] ->
                        List.map Open question

                    _ ->
                        l
           )


proContra : DNF -> List Proposition -> Maybe { pro : List Argument, contra : List Argument }
proContra question information =
    let
        pro =
            arguments question information

        contra =
            arguments (negate_ question) information
    in
    case ( pro, contra ) of
        ( [], [] ) ->
            Nothing

        _ ->
            Just
                { pro = pro
                , contra = contra
                }


preferred : Preference -> Argument -> Argument -> Bool
preferred preference a b =
    case ( a, b ) of
        ( Open _, Open _ ) ->
            False

        ( Open _, _ ) ->
            False

        ( _, Open _ ) ->
            True

        ( a_, b_ ) ->
            Maybe.map2 (\ha hb -> preference ha hb)
                (head a_)
                (head b_)
                |> Maybe.join
                |> Maybe.withDefault False


isDefeated : Preference -> Argument -> Bool
isDefeated preference a =
    case a of
        Argument _ { pro, contra } ->
            (pro
                |> List.filter (\p -> not (isDefeated preference p) && not (isRebutted preference contra p))
                |> (==) []
            )
                || (contra
                        |> List.filter (\p -> not (isDefeated preference p) && not (isRebutted preference pro p))
                        |> List.any (\p -> preferred preference p a)
                   )

        _ ->
            False


isRebutted : Preference -> List Argument -> Argument -> Bool
isRebutted preference opponents a =
    opponents
        |> List.filter (\p -> not (isDefeated preference p))
        |> List.any (\b -> preferred preference b a)


argumentToRelevantArgument : Preference -> Argument -> RelevantArgument
argumentToRelevantArgument preference a =
    case a of
        Assumption h ->
            RelevantAssumption h

        Argument h l ->
            RelevantArgument h (winnersLosers preference l)

        Open c ->
            RelevantOpen c


argumentToIrrelevantArgument : Argument -> Defeated
argumentToIrrelevantArgument a =
    case a of
        Assumption h ->
            DefeatedClosed h

        Argument h _ ->
            DefeatedClosed h

        Open c ->
            DefeatedOpen c


winnersLosers :
    Preference
    ->
        { pro : List Argument
        , contra : List Argument
        }
    -> Support
winnersLosers preference { pro, contra } =
    let
        ( proLosers, proWinners ) =
            List.partition (\a -> isRebutted preference contra a || isDefeated preference a) pro

        ( contraLosers, contraWinners ) =
            List.partition (\a -> isRebutted preference pro a || isDefeated preference a) contra
    in
    { relevant =
        { pro = List.map (argumentToRelevantArgument preference) proWinners
        , contra = List.map (argumentToRelevantArgument preference) contraWinners
        }
    , irrelevant =
        { pro = List.map argumentToIrrelevantArgument proLosers
        , contra = List.map argumentToIrrelevantArgument contraLosers
        }
    }


explanation : Preference -> Proposition -> List Proposition -> Maybe Support
explanation preference question information =
    proContra (cases question) information |> Maybe.map (winnersLosers preference)


combineOpenArguments : { pro : List RelevantArgument, contra : List RelevantArgument } -> DNF
combineOpenArguments { pro, contra } =
    (openArguments pro ++ negate_ (openArguments contra))
        |> List.uniqueBy (List.map factToString)


openArguments : List RelevantArgument -> DNF
openArguments =
    List.map
        (\a ->
            case a of
                RelevantArgument _ { relevant } ->
                    combineOpenArguments relevant

                RelevantOpen c ->
                    [ c ]

                RelevantAssumption _ ->
                    []
        )
        >> List.concat
