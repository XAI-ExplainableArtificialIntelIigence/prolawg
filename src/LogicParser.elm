module LogicParser exposing (..)

import Parser exposing ((|.), (|=), Parser, float, keyword, symbol)
import Pratt exposing (constant, infixLeft, infixRight, literal, postfix, prefix)
import Set
import Types exposing (..)


logicExpression : Parser Proposition
logicExpression =
    Pratt.expression
        { oneOf =
            [ variable, parenthesizedExpression ]
                ++ List.map (\a -> constant (keyword a) False_)
                    [ "true", "True", "TRUE", "1" ]
                ++ List.map (\a -> constant (keyword a) False_)
                    [ "false", "False", "FALSE", "0" ]
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
                ++ List.map (\a -> infixLeft 1 (symbol a) (\p q -> And (Implies p q) (Implies q p)))
                    [ "↔"]
        , spaces = Parser.spaces
        }


variable : Pratt.Config Proposition -> Parser Proposition
variable _ =
    Parser.succeed Variable
        |= Parser.variable
            { start = Char.isAlphaNum
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


parse : String -> Maybe Proposition
parse text =
    let
        unambiguous =
            text
                |> String.replace " " ""
                |> String.replace "<->" "↔"
                |> String.replace ":-:" "↔"
                |> String.replace "iff" "↔"
                |> String.replace "Iff" "↔"
                |> String.replace "IFF" "↔"
    in
    case Parser.run logic unambiguous of
        Ok a ->
            Just a

        Err _ ->
            Nothing
