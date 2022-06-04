module Enclojure.Reader exposing (parse)

import Array
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader.DoubleQuotedString as DoubleQuotedString
import Enclojure.Reader.Macros as Macros
import Enclojure.Common exposing (Exception(..), Number(..), Value(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMapEntry)
import Enclojure.ValueSet as ValueSet
import Parser exposing ((|.), (|=), Parser)
import Regex
import Set


located : Parser a -> Parser (Located a)
located p =
    Parser.succeed (\start v end -> Located.at start end v)
        |= Parser.getPosition
        |= p
        |= Parser.getPosition


parse : String -> Result (List Parser.DeadEnd) (List (Located (Value io)))
parse code =
    Parser.run parser code


negateNumber : Number -> Number
negateNumber num =
    case num of
        Int v ->
            Int -v

        Float v ->
            Float -v


positiveNumber : Parser Number
positiveNumber =
    Parser.number
        { int = Just Int
        , float = Just Float
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        }


number : Parser (Value io)
number =
    Parser.oneOf
        [ Parser.succeed (Number << negateNumber)
            |. Parser.symbol "-"
            |= positiveNumber
        , Parser.map Number positiveNumber
        ]
        |> Parser.backtrackable


isAllowedSymbolSpecialChar : Char -> Bool
isAllowedSymbolSpecialChar c =
    c
        == '-'
        || c
        == '+'
        || c
        == '/'
        || c
        == '-'
        || c
        == '*'
        || c
        == '>'
        || c
        == '<'
        || c
        == '='
        || c
        == '\''
        || c
        == '&'
        || c
        == '%'
        || c
        == '?'
        || c
        == '.'
        || c
        == '$'
        || c
        == '_'
        || c
        == '!'


symbolLike : Parser String
symbolLike =
    Parser.variable
        { start =
            \c ->
                Char.isAlpha c
                    || isAllowedSymbolSpecialChar c
        , inner = \c -> Char.isAlphaNum c || isAllowedSymbolSpecialChar c
        , reserved = Set.empty
        }


symbol : Parser (Value io)
symbol =
    Parser.succeed
        (\token ->
            case token of
                "nil" ->
                    Nil

                "true" ->
                    Bool True

                "false" ->
                    Bool False

                _ ->
                    Symbol token
        )
        |= symbolLike


keyword : Parser (Value io)
keyword =
    Parser.succeed Keyword
        |. Parser.symbol ":"
        |= symbolLike


expressionsHelper : List (Located (Value io)) -> Parser (Parser.Step (List (Located (Value io))) (List (Located (Value io))))
expressionsHelper revExprs =
    Parser.oneOf
        [ Parser.succeed (\expr -> Parser.Loop (expr :: revExprs))
            |= located expression
            |. spaces
        , Parser.succeed (\_ -> Parser.Loop revExprs)
            |= lineComment
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revExprs))
        ]


lineComment : Parser ()
lineComment =
    Parser.lineComment ";"


uncommentedExpression : Parser (Value io)
uncommentedExpression =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ lambda
            , regex
            , string
            , list
            , vector
            , valueMap
            , valueSet
            , number
            , symbol
            , keyword
            ]
        |. Parser.spaces


wrapInQuote : Value io -> Value io
wrapInQuote val =
    List [ Located.unknown (Symbol "quote"), Located.unknown val ]


expression : Parser (Value io)
expression =
    Parser.oneOf
        [ Parser.backtrackable <|
            Parser.succeed identity
                |. Parser.spaces
                |. Parser.token "#_"
                |. Parser.spaces
                |. uncommentedExpression
                |= uncommentedExpression
        , Parser.backtrackable <|
            Parser.succeed wrapInQuote
                |. Parser.spaces
                |. Parser.symbol "'"
                |. Parser.spaces
                |= uncommentedExpression
        , uncommentedExpression
        ]


spaces : Parser ()
spaces =
    Parser.oneOf
        [ Parser.spaces
        , Parser.chompIf (\c -> c == ',')
        ]


vector : Parser (Value io)
vector =
    Parser.sequence
        { start = "["
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = "]"
        }
        |> Parser.map (Array.fromList >> Vector)


lambda : Parser (Value io)
lambda =
    Parser.sequence
        { start = "#("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> located
        |> Parser.map (\(Located loc v) -> List (Located loc (Symbol "__lambda") :: v))


list : Parser (Value io)
list =
    Parser.sequence
        { start = "("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> Parser.map List


mapEntry : Parser (ValueMapEntry io)
mapEntry =
    Parser.succeed Tuple.pair
        |= expression
        |. spaces
        |= located expression


valueMap : Parser (Value io)
valueMap =
    Parser.sequence
        { start = "{"
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> mapEntry)
        , trailing = Parser.Optional
        , end = "}"
        }
        |> Parser.map (ValueMap.fromList >> Map)


valueSet : Parser (Value io)
valueSet =
    Parser.sequence
        { start = "#{"
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> expression)
        , trailing = Parser.Optional
        , end = "}"
        }
        |> Parser.map (ValueSet.fromList >> Set)


string : Parser (Value io)
string =
    DoubleQuotedString.string |> Parser.map String


parser : Parser (List (Located (Value io)))
parser =
    Parser.loop [] expressionsHelper
        |> Parser.andThen
            (\l ->
                List.foldr (\e a -> a |> Result.andThen (\lr -> Macros.macroexpandAll e |> Result.map (\v -> v :: lr)))
                    (Ok [])
                    l
                    |> (\r ->
                            case r of
                                Ok v ->
                                    Parser.succeed v

                                Err (Exception e _) ->
                                    Parser.problem e
                       )
            )


regex : Parser (Value io)
regex =
    (Parser.succeed identity
        |. (Parser.token "#" |> Parser.backtrackable)
        |= DoubleQuotedString.string
    )
        |> Parser.andThen
            (\pattern ->
                Regex.fromString pattern
                    |> Maybe.map (Regex pattern >> Parser.succeed)
                    |> Maybe.withDefault (Parser.problem "invalid regex")
            )
