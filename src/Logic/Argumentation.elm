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


head : Argument -> Proposition
head a =
    case a of
        Assumption h ->
            h

        Argument h _ ->
            h


type RelevantArgument
    = RelevantAssumption Proposition
    | RelevantArgument Proposition Support


type alias Support =
    { relevant :
        { pro : List RelevantArgument
        , contra : List RelevantArgument
        }
    , irrelevant :
        { pro : List Proposition
        , contra : List Proposition
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


isDefeated : Preference -> Argument -> Bool
isDefeated preferred a =
    case a of
        Assumption _ ->
            False

        Argument h { pro, contra } ->
            (pro
                |> List.filter (\p -> not (isDefeated preferred p) && not (isRebutted preferred contra p))
                |> (==) []
            )
                || (contra
                        |> List.filter (\p -> not (isDefeated preferred p) && not (isRebutted preferred pro p))
                        |> List.any (\p -> Maybe.withDefault False (preferred (head p) h))
                   )


isRebutted : Preference -> List Argument -> Argument -> Bool
isRebutted preferred opponents a =
    opponents
        |> List.filter (\p -> not (isDefeated preferred p))
        |> List.any (\b -> Maybe.withDefault False (preferred (head b) (head a)))


winnersLosers :
    Preference
    ->
        { pro : List Argument
        , contra : List Argument
        }
    -> Support
winnersLosers preference { pro, contra } =
    let
        candidates =
            List.map (\a -> ( True, a )) pro ++ List.map (\a -> ( False, a )) contra

        ( losers, winners ) =
            List.partition
                (\( isPro, a ) ->
                    let
                        opponents =
                            List.filter (\( isPro_, _ ) -> isPro_ == isPro) candidates
                                |> List.map Tuple.second
                    in
                    isRebutted preference opponents a || isDefeated preference a
                )
                candidates
    in
    { relevant =
        let
            ( pro_, contra_ ) =
                winners
                    |> List.map
                        (\( isPro, a ) ->
                            case a of
                                Assumption h ->
                                    ( isPro, RelevantAssumption h )

                                Argument h l ->
                                    ( isPro, RelevantArgument h (winnersLosers preference l) )
                        )
                    |> List.partition Tuple.first
        in
        { pro = List.map Tuple.second pro_, contra = List.map Tuple.second contra_ }
    , irrelevant =
        let
            ( pro_, contra_ ) =
                losers |> List.map (\( isPro, a ) -> ( isPro, head a )) |> List.partition Tuple.first
        in
        { pro = List.map Tuple.second pro_, contra = List.map Tuple.second contra_ }
    }


explanation : Preference -> Proposition -> List Proposition -> Maybe Support
explanation preference question information =
    proContra (cases question) information |> Maybe.map (winnersLosers preference)



-- STRINGIFICATION


argumentToString : Argument -> String
argumentToString a =
    case a of
        Assumption p ->
            propositionToString p

        Argument p { pro, contra } ->
            "(pro: ["
                ++ (pro
                        |> List.map argumentToString
                        |> List.sort
                        |> String.join ", "
                   )
                ++ "], contra: ["
                ++ (contra
                        |> List.map argumentToString
                        |> List.sort
                        |> String.join ", "
                   )
                ++ "], "
                ++ propositionToString p
                ++ ")"
