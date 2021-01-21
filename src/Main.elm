module Main exposing (..)

import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra as List
import Logic.Argumentation exposing (..)
import Logic.Core exposing (..)
import Logic.Parser exposing (parseRanked)
import Maybe.Extra as Maybe


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { rulesInput : String
    , questionInput : String
    , rules : Maybe ( List Proposition, Preference )
    , question : Maybe Proposition
    , showDefeated : Bool
    }


init : Model
init =
    { rulesInput = String.join "\n" initialRules
    , questionInput = initialQuestion
    , rules = initialRules |> List.map parseRanked |> Maybe.combine |> Maybe.map rankingToPreference
    , question = Maybe.map Tuple.second (parseRanked initialQuestion)
    , showDefeated = False
    }


initialQuestion =
    "x"


initialRules =
    [ "2: a /\\ b -> x"

    --, "a and b"
    , "1: c -> not x"

    --, "not c"
    ]



-- UPDATE


type Msg
    = NewRules String
    | NewQuestion String
    | LoadExample ( String, List String )
    | AddRule Proposition
    | ShowDefeatedChecked Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewRules s ->
            { model
                | rulesInput = s
                , rules =
                    s
                        |> String.split "\n"
                        |> List.filter (\a -> String.replace " " "" a /= "")
                        |> List.map parseRanked
                        |> Maybe.combine
                        |> Maybe.map rankingToPreference
            }

        NewQuestion s ->
            { model
                | questionInput = s
                , question = Maybe.map Tuple.second (parseRanked s)
            }

        LoadExample ( question, rules ) ->
            { model
                | rulesInput = String.join "\n" rules ++ "\n"
                , rules = rules |> List.map parseRanked |> Maybe.combine |> Maybe.map rankingToPreference
                , questionInput = question
                , question = Maybe.map Tuple.second (parseRanked question)
            }

        AddRule a ->
            { model
                | rulesInput = model.rulesInput ++ "\n" ++ propositionToString a
                , rules =
                    case model.rules of
                        Nothing ->
                            Just ( [ a ], \_ _ -> Nothing )

                        Just ( l, p ) ->
                            Just ( l ++ [ a ], p )
            }

        ShowDefeatedChecked b ->
            { model | showDefeated = b }



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ logicFont
        , paddingXY 0 50
        ]
        (column
            [ width (px 800)
            , spacing 50
            , centerX
            ]
            [ Input.multiline [ logicFont ]
                { onChange = NewRules
                , text = model.rulesInput
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Rules, optionally prefixed with priority:")
                , spellcheck = False
                }
            , case model.rules of
                Nothing ->
                    text "Error parsing rules."

                _ ->
                    none
            , Input.text [ logicFont ]
                { onChange = NewQuestion
                , text = model.questionInput
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Question:")
                }
            , case ( model.rules, model.question ) of
                ( _, Nothing ) ->
                    text "Error parsing question."

                ( Just ( rules, preference ), Just question ) ->
                    let
                        { winners, losers } =
                            explanation question ( rules, preference )
                    in
                    column [ spacing 50 ]
                        [ column [ spacing 20, width fill ]
                            (viewQuestions preference { winners = winners, losers = losers })
                        , column [ spacing 20 ]
                            [ text "Explanations:"
                            , Input.checkbox []
                                { onChange = ShowDefeatedChecked
                                , icon = Input.defaultCheckbox
                                , checked = model.showDefeated
                                , label =
                                    Input.labelRight []
                                        (text "Show defeated arguments?")
                                }
                            , column [ width fill, spacing 2 ]
                                (if winners.pro == [] && winners.contra == [] then
                                    [ text "There are no pro or contra arguments." ]

                                 else
                                    viewArguments preference { winners = winners, losers = losers } 0 True model.showDefeated
                                        ++ [ explanationText ]
                                )
                            ]
                        ]

                _ ->
                    none
            , el
                [ Font.color (rgb 0 0 1)
                , Events.onClick (LoadExample employment)
                , pointer
                ]
                (text "Load employment law example.")
            ]
        )


viewQuestions : Preference -> { winners : Support, losers : Support } -> List (Element Msg)
viewQuestions preference supports =
    let
        questions_ =
            questions preference supports
    in
    if questions_ == [] then
        []

    else
        [ paragraph [] [ text "Further information required!" ]
        , paragraph [] [ text "Answer at least one of the questions in each block." ]
        , column [ width fill, spacing 20 ]
            (List.map
                (\l ->
                    column
                        [ Border.width 2
                        , padding 10
                        , width fill
                        , spacing 20
                        ]
                        (List.map viewQuestion l)
                )
                questions_
            )
        ]


viewQuestion a =
    row [ spacing 40 ]
        [ text (a ++ "?")
        , el
            [ Events.onClick (AddRule (Variable a))
            , pointer
            ]
            (text "True")
        , el
            [ Events.onClick (AddRule (Not (Variable a)))
            , pointer
            ]
            (text "False")
        ]


viewArgument : Preference -> Int -> Bool -> Bool -> Bool -> Argument -> Element Msg
viewArgument preference depth isPro isWinner showDefeated argument =
    column
        [ width fill
        , spacing 2
        ]
        (case argument of
            Assumption p ->
                [ indented depth isPro isWinner False (text (propositionToString p)) ]

            Argument p s ->
                indented depth isPro isWinner False (text (propositionToString p))
                    :: viewArguments preference (winnersLosers preference s) (depth + 1) isPro showDefeated

            Open c ->
                [ indented depth isPro isWinner True (text (String.join ", " (List.map factToString c))) ]
        )


viewArguments : Preference -> { winners : Support, losers : Support } -> Int -> Bool -> Bool -> List (Element Msg)
viewArguments preference { winners, losers } depth isPro showDefeated =
    (let
        { pro, contra } =
            winners
     in
     List.map (viewArgument preference depth isPro True showDefeated) pro
        ++ List.map (viewArgument preference depth (not isPro) True showDefeated) contra
    )
        ++ (let
                { pro, contra } =
                    losers
            in
            if showDefeated then
                List.map (viewArgument preference depth isPro False False) pro
                    ++ List.map (viewArgument preference depth (not isPro) False False) contra

            else
                []
           )


indented depth isPro isWinner isOpen x =
    let
        color =
            if isPro then
                rgba 0 0 0 ((toFloat depth + 1) * 0.05)

            else
                rgba 0 0 0 (1 - ((toFloat depth + 1) * 0.05))
    in
    row [ width fill ]
        [ column
            [ width (px (depth * 20))
            ]
            []
        , column
            ([ width fill
             , Background.color
                (if isOpen then
                    rgb 1 1 1

                 else
                    color
                )
             , Border.width 2
             , Border.color color
             , if isOpen then
                Border.dotted

               else
                Border.solid
             , Font.color
                (if isPro || isOpen then
                    rgb 0 0 0

                 else
                    rgb 1 1 1
                )
             , padding 5
             ]
                ++ (if isWinner then
                        []

                    else
                        [ Font.strike ]
                   )
            )
            [ x ]
        ]


marginLeft : Int -> Element.Attribute msg
marginLeft x =
    htmlAttribute (style "margin-left" (String.fromInt x ++ "px"))


logicFont : Element.Attribute Msg
logicFont =
    Font.family
        [ Font.external
            { name = "FiraLogic"
            , url = "font.css"
            }
        , Font.sansSerif
        ]


explanationText =
    column [ paddingXY 0 20, spacingXY 0 20 ]
        [ paragraph []
            [ text "Pro arguments are "
            , el [ Border.width 2, Border.color (rgba 0 0 0 0.1), Background.color (rgba 0 0 0 0.1), padding 5 ] (text "gray")
            , text ", contra arguments are "
            , el [ Border.width 2, Font.color (rgb 1 1 1), Background.color (rgb 0 0 0), padding 5 ] (text "black")
            , text "."
            ]
        , paragraph []
            [ text "Information that is not yet known is in "
            , el
                [ padding 5
                , Border.width 2
                , Border.color (rgba 0 0 0 0.1)
                , Border.dotted
                ]
                (text "dotted")
            , text " "
            , el
                [ padding 5
                , Border.width 2
                , Border.dotted
                ]
                (text "frames")
            , text "."
            ]
        , paragraph []
            [ text "Possibly relevant arguments that are rebutted or defeated are "
            , el [ Font.strike ] (text "struck through")
            , text "."
            ]
        , paragraph []
            [ text "There is some redundancy when arguments can be ordered in different ways."
            ]
        ]



-- DATA


employment =
    ( "CanMakeRequestForChange"
    , [ "Employed"
      , "¬LessThanTenEmployees"
      , "¬ReachedOldAgeInsurance"
      , "MilitaryOfficial"
      , "WorkedForAtLeastTwentySixWeeks"
      , "Employed -> CanMakeRequestForChange"
      , "Employed /\\ LessThanTenEmployees -> ¬CanMakeRequestForChange"
      , "Employed /\\ ReachedOldAgeInsurance -> ¬CanMakeRequestForChange"
      , "Employed /\\ MilitaryOfficial -> ¬CanMakeRequestForChange"
      ]
    )
