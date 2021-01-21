module Logic.Parser exposing (..)

import Logic.Core exposing (..)
import Parser exposing ((|.), (|=), Parser, int, keyword, oneOf, spaces, symbol)
import Pratt exposing (constant, infixLeft, prefix)
import Set


logicExpression : Parser Proposition
logicExpression =
    Pratt.expression
        { oneOf =
            [ variable, parenthesizedExpression ]
                ++ List.map (\a -> constant (keyword a) False_)
                    [ "true", "True", "TRUE" ]
                ++ List.map (\a -> constant (keyword a) False_)
                    [ "false", "False", "FALSE" ]
                ++ List.map (\a -> prefix 5 (symbol a) Not)
                    [ "¬", "~", "!", "not", "Not", "NOT" ]
        , andThenOneOf =
            List.map (\a -> infixLeft 4 (symbol a) And)
                [ "∧", "/\\", ",", "&", "&&", "and", "And", "AND" ]
                ++ List.map (\a -> infixLeft 3 (symbol a) Or)
                    [ "∨", "\\/", "|", "||", "or", "Or", "OR" ]
                ++ List.map (\a -> infixLeft 2 (symbol a) Implies)
                    [ "→", "->", "-:", "implies", "Implies", "IMPLIES" ]
                ++ List.map (\a -> infixLeft 2 (symbol a) (\p q -> Implies q p))
                    [ "←", "<-", ":-", "if", "If", "IF" ]
                ++ List.map (\a -> infixLeft 1 (symbol a) Equiv)
                    [ "↔" ]
        , spaces = Parser.spaces
        }


variable : Pratt.Config Proposition -> Parser Proposition
variable _ =
    Parser.succeed Variable
        |= Parser.variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "not" ]
            }


parenthesizedExpression : Pratt.Config Proposition -> Parser Proposition
parenthesizedExpression config =
    Parser.succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


logic : Parser Proposition
logic =
    Parser.succeed identity
        |= logicExpression
        -- string must end after an expression:
        |. Parser.end


parse_ : Parser a -> String -> Maybe a
parse_ parser text =
    let
        unambiguous =
            text
                --|> String.replace " " ""
                |> String.replace "<->" "↔"
                |> String.replace ":-:" "↔"
                |> String.replace "iff" "↔"
                |> String.replace "Iff" "↔"
                |> String.replace "IFF" "↔"
    in
    Parser.run parser unambiguous
        |> Result.toMaybe


rankedLogic : Parser ( Int, Proposition )
rankedLogic =
    Parser.oneOf
        [ Parser.succeed (\a -> ( 0, a ))
            |= logicExpression
            |. Parser.end
        , Parser.succeed (\a b -> ( a, b ))
            |= int
            |. oneOf
                [ symbol ":"
                , spaces
                ]
            |. spaces
            |= logicExpression
            |. Parser.end
        , Parser.succeed (\a b -> ( -a, b ))
            |. symbol "-"
            |= int
            |. oneOf
                [ symbol ":"
                , spaces
                ]
            |. spaces
            |= logicExpression
            |. Parser.end
        ]


parseRanked : String -> Maybe ( Int, Proposition )
parseRanked =
    parse_ rankedLogic
