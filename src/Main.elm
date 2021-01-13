module Main exposing (..)

import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra as List
import Logic exposing (..)
import LogicParser exposing (parse)
import Maybe.Extra as Maybe
import Types exposing (..)


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


init : Model
init =
    { rulesInput = String.join "\n" initialRules
    , questionInput = initialQuestion
    , rules = initialRules |> List.map parse |> Maybe.combine |> Maybe.map preferenceFromPriority
    , question = Maybe.map Tuple.second (parse initialQuestion)
    }


initialQuestion =
    "x"


initialRules =
    [ "1: a /\\ b -> x"
    , "a and b"
    , "2: c -> not x"
    , "not c"
    ]


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
                    column
                        [ width fill
                        , spacingXY 0 20
                        ]
                        [ paragraph [] [ text "Explanations:" ]
                        , column [ width fill, spacing 2 ]
                            (explanation preference question rules
                                |> Maybe.map
                                    (\{ relevant, irrelevant } ->
                                        viewArguments relevant irrelevant 0 True
                                            ++ [ explanationText ]
                                    )
                                |> Maybe.withDefault [ text "Some information is missing." ]
                            )
                        ]

                _ ->
                    none
            ]
        )


explanationText =
    column [ paddingXY 0 20, spacingXY 0 20 ]
        [ paragraph [] [ text "Pro arguments are ", el [ Background.color (rgba 0 0 0 0.1), padding 5 ] (text "gray"), text ", contra arguments are ", el [ Font.color (rgb 1 1 1), Background.color (rgb 0 0 0), padding 5 ] (text "black"), text "." ]
        , paragraph [] [ text "Possibly relevant arguments that are rebutted or defeated are ", el [ Font.strike ] (text "striked through"), text "." ]

        -- text ". (Information on their defeaters could be displayed in principle, but that would blow the explanation scheme, since the defeaters also depend on pro and contra arguments, etc.)" ]
        , paragraph [] [ text "There is some redundancy when arguments can be ordered in different ways." ]
        ]


viewExplanation : Int -> Bool -> RelevantArgument -> Element msg
viewExplanation depth isPro a =
    column
        [ width fill
        , spacing 2
        ]
        (case a of
            RelevantAssumption p ->
                [ indented depth isPro True (text (string.fromProposition p)) ]

            RelevantArgument p { relevant, irrelevant } ->
                indented depth isPro True (text (string.fromProposition p))
                    :: viewArguments relevant irrelevant (depth + 1) isPro
        )


viewArguments relevant irrelevant depth isPro =
    (let
        { pro, contra } =
            relevant
     in
     List.map (viewExplanation depth isPro) pro
        ++ List.map (viewExplanation depth (not isPro)) contra
    )
        ++ (let
                { pro, contra } =
                    irrelevant
            in
            List.map (\q -> indented depth isPro False (text (string.fromProposition q))) pro
                ++ List.map (\q -> indented depth (not isPro) False (text (string.fromProposition q))) contra
           )


indented depth isPro isRelevant x =
    row [ width fill ]
        [ column
            [ width (px (depth * 20))
            ]
            []
        , column
            ([ width fill
             , Background.color
                (rgba 0
                    0
                    0
                    (if isPro then
                        (toFloat depth + 1) * 0.05

                     else
                        1 - ((toFloat depth + 1) * 0.05)
                    )
                )
             , Font.color
                (if isPro then
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


rank j =
    if j == 0 then
        ""

    else
        String.fromInt j ++ ": "


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
                        |> List.map parse
                        |> Maybe.combine
                        |> Maybe.map preferenceFromPriority
            }

        NewQuestion s ->
            { model
                | questionInput = s
                , question = Maybe.map Tuple.second (parse s)
            }

        LoadExample ( question, rules ) ->
            { model
                | rulesInput = String.join "\n" rules ++ "\n"
                , rules = rules |> List.map parse |> Maybe.combine |> Maybe.map preferenceFromPriority
                , questionInput = question
                , question = Maybe.map Tuple.second (parse question)
            }


preferenceFromPriority : List ( Int, Proposition ) -> ( List Proposition, Preference )
preferenceFromPriority l =
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



-- a and b -> x
-- a
-- b
-- c -> not x
-- (a and b) or c
--
-- a and b -> x
-- a
-- b
-- c -> not x
-- (a and b) or c
