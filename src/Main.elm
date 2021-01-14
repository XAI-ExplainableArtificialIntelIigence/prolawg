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


type alias Model =
    { rulesInput : String
    , questionInput : String
    , rules : Maybe ( List Proposition, Preference )
    , question : Maybe Proposition
    }


type Msg
    = NewRules String
    | NewQuestion String
    | LoadExample ( String, List String )



-- MODEL


init : Model
init =
    { rulesInput = String.join "\n" initialRules
    , questionInput = initialQuestion
    , rules = initialRules |> List.map parseRanked |> Maybe.combine |> Maybe.map parseExample
    , question = Maybe.map Tuple.second (parseRanked initialQuestion)
    }


initialQuestion =
    "x"


initialRules =
    [ "1: a /\\ b -> x"
    , "a and b"
    , "2: c -> not x"
    , "not c"
    ]



-- UPDATE


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
                        |> Maybe.map parseExample
            }

        NewQuestion s ->
            { model
                | questionInput = s
                , question = Maybe.map Tuple.second (parseRanked s)
            }

        LoadExample ( question, rules ) ->
            { model
                | rulesInput = String.join "\n" rules ++ "\n"
                , rules = rules |> List.map parseRanked |> Maybe.combine |> Maybe.map parseExample
                , questionInput = question
                , question = Maybe.map Tuple.second (parseRanked question)
            }



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
            , el
                [ Font.color (rgb 0 0 1)
                , Events.onClick (LoadExample employment)
                , pointer
                ]
                (text "Load employment law example.")
            , Input.text [ logicFont ]
                { onChange = NewQuestion
                , text = model.questionInput
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Question:")
                }
            , case model.question of
                Nothing ->
                    text "Error parsing question."

                _ ->
                    none
            , case ( model.rules, model.question ) of
                ( Just ( rules, preference ), Just question ) ->
                    let
                        explanation_ =
                            explanation preference question rules
                    in
                    column
                        [ width fill
                        , spacingXY 0 20
                        ]
                        ((case
                            Maybe.map
                                (List.filter (\l -> List.length l > 0)
                                    << combineOpenArguments
                                    << .relevant
                                )
                                explanation_
                          of
                            Just (h :: t) ->
                                let
                                    _ =
                                        Debug.log "h :: t" (h :: t)
                                in
                                [ paragraph [] [ text "Further information required!" ]
                                , paragraph [] [ text "Answer at least one of the questions in each block." ]
                                , column [ width fill, spacing 20 ]
                                    ((h :: t)
                                        |> transform
                                        |> List.sortBy List.length
                                        |> List.map
                                            (\l ->
                                                column 
                                                    [ Border.width 2
                                                    , padding 10
                                                    , width fill
                                                    , spacing 20
                                                    ]
                                                    (text "Please answer one of the following questions: "
                                                        :: List.map (\a -> text (variable a ++ "?")) l
                                                    )
                                            )
                                    )
                                ]

                            _ ->
                                []
                         )
                            ++ [ paragraph [] [ text "Explanations:" ]
                               , column [ width fill, spacing 2 ]
                                    (explanation_
                                        |> Maybe.map
                                            (\s ->
                                                viewArguments s 0 True
                                                    ++ [ explanationText ]
                                            )
                                        |> Maybe.withDefault [ text "Some information is missing." ]
                                    )
                               ]
                        )

                _ ->
                    none
            ]
        )


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
            , el [ padding 5, Border.width 2, Border.color (rgba 0 0 0 0.1) ] (text "bordered")
            , text " "
            , el [ padding 5, Border.width 2 ] (text "boxes")
            , text "."
            ]
        , paragraph []
            [ text "Possibly relevant arguments that are rebutted or defeated are "
            , el [ Font.strike ] (text "striked through")
            , text "."
            ]
        , paragraph []
            [ text "There is some redundancy when arguments can be ordered in different ways."
            ]
        ]


viewRelevantArgument : Int -> Bool -> RelevantArgument -> Element Msg
viewRelevantArgument depth isPro a =
    column
        [ width fill
        , spacing 2
        ]
        (case a of
            RelevantAssumption p ->
                [ indented depth isPro True False (text (propositionToString p)) ]

            RelevantArgument p s ->
                indented depth isPro True False (text (propositionToString p))
                    :: viewArguments s (depth + 1) isPro

            RelevantOpen c ->
                [ indented depth isPro True True (text (String.join ", " (List.map factToString c))) ]
        )


viewDefeatedArgument : Int -> Bool -> Defeated -> Element Msg
viewDefeatedArgument depth isPro a =
    column
        [ width fill
        , spacing 2
        ]
        (case a of
            DefeatedClosed p ->
                [ indented depth isPro False False (text (propositionToString p)) ]

            DefeatedOpen c ->
                [ indented depth isPro False True (text (String.join ", " (List.map factToString c))) ]
        )


viewArguments : Support -> Int -> Bool -> List (Element Msg)
viewArguments { relevant, irrelevant } depth isPro =
    (let
        { pro, contra } =
            relevant
     in
     List.map (viewRelevantArgument depth isPro) pro
        ++ List.map (viewRelevantArgument depth (not isPro)) contra
    )
        ++ (let
                { pro, contra } =
                    irrelevant
            in
            List.map (viewDefeatedArgument depth isPro) pro
                ++ List.map (viewDefeatedArgument depth isPro) contra
           )


indented depth isPro isRelevant isOpen x =
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
             , Font.color
                (if isPro || isOpen then
                    rgb 0 0 0

                 else
                    rgb 1 1 1
                )
             , padding 5
             ]
                ++ (if isRelevant then
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



-- DATA


parseExample : List ( Int, Proposition ) -> ( List Proposition, Preference )
parseExample l =
    ( List.map Tuple.second l
    , \a b ->
        l
            |> List.find (\( _, p ) -> p == a)
            |> Maybe.map
                (\( i, _ ) ->
                    l
                        |> List.find (\( _, q ) -> q == b)
                        |> Maybe.map (\( j, _ ) -> i > j)
                )
            |> Maybe.join
    )


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
