module Enclojure.Lib.String exposing (init)

import Enclojure.Callable as Callable
import Enclojure.Common as Common exposing (Arity(..), Callable, Exception(..), IO(..), Value(..))
import Enclojure.Located as Located
import Enclojure.Runtime as Runtime
import Enclojure.Value as Value exposing (inspect)
import Regex


emptyCallable : Callable io
emptyCallable =
    Callable.new


init : Common.Env io -> Common.Env io
init env =
    [ ( "string/blank?", isBlank )
    , ( "string/capitalize", capitalize )
    , ( "string/ends-with?", endsWith )
    , ( "string/includes?", includes )
    , ( "string/index-of", indexOf )
    , ( "string/join", join )
    , ( "string/last-index-of", lastIndexOf )
    , ( "string/length", length )
    , ( "string/lower-case", lowerCase )
    , ( "string/replace", replace )
    , ( "string/replace-first", replaceFirst )
    , ( "string/reverse", reverse )
    , ( "string/split-lines", splitLines )
    , ( "string/split", split )
    , ( "string/starts-with?", startsWith )
    , ( "string/trim", trim )
    , ( "string/triml", triml )
    , ( "string/trimr", trimr )
    , ( "string/upper-case", upperCase )
    ]
        |> List.foldl
            (\( name, fn ) ->
                Runtime.bindGlobal name (Fn { name = Just name, doc = Nothing, signatures = Callable.signatures fn } (Common.toThunk fn))
            )
            env


splitLines : Common.Callable io
splitLines =
    let
        arity1 val =
            case val of
                String s ->
                    String.lines s
                        |> List.map (String >> Located.unknown)
                        |> Common.List
                        |> Ok

                _ ->
                    Err (Exception ("type error: expected string, got " ++ inspect val) [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction (arity1 >> Result.map Const)
        , doc = Just "Splits s on \\n or \\r\\n. Trailing empty lines are not returned."
    }


length : Common.Callable io
length =
    let
        arity1 val =
            Value.tryString val
                |> Maybe.map (String.length >> Common.Int >> Common.Number >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected string, got " ++ inspect val) []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction (arity1 >> Result.map Const)
        , doc = Just "Returns the length of string s."
    }


join : Common.Callable io
join =
    let
        arity1 val =
            val
                |> Value.trySequenceOf (Value.toString >> Just)
                |> Maybe.map (String.join "" >> Common.String >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected a sequence, got " ++ inspect val) []))

        arity2 ( sepVal, collVal ) =
            Maybe.map2 (\sep coll -> String.join sep coll |> Common.String |> Ok)
                (Value.tryString sepVal)
                (Value.trySequenceOf (Value.toString >> Just) collVal)
                |> Maybe.withDefault (Err (Exception "type error: expected a separator and a sequence of strings" []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction (arity1 >> Result.map Const)
        , arity2 = Just <| Fixed ( Symbol "sep", Symbol "coll" ) <| Callable.toArityFunction (arity2 >> Result.map Const)
        , doc = Just "Returns a string of all elements in coll, as returned by (seq coll), separated by an optional separator."
    }


isBlank : Common.Callable io
isBlank =
    let
        arity1 sVal =
            case sVal of
                String s ->
                    s |> String.trim |> String.isEmpty |> Bool |> Const |> Ok

                Nil ->
                    Ok <| Const <| Bool True

                _ ->
                    Err (Value.exception "type error: blank? expects a string or nil")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "True if s is nil, empty, or contains only whitespace."
    }


capitalize : Common.Callable io
capitalize =
    let
        arity1 value =
            value
                |> Value.tryString
                |> Maybe.map
                    (\s ->
                        String.uncons s
                            |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
                            |> Maybe.withDefault (String.toUpper s)
                            |> String
                            |> Const
                    )
                |> Result.fromMaybe (Value.exception "type error: capitalize expects a string")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Converts first character of the string to upper-case, all other characters to lower-case."
    }


endsWith : Common.Callable io
endsWith =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.endsWith substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: ends-with? expects a string")
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "substr" ) <| Callable.toArityFunction arity2
        , doc = Just "True if s ends with substr."
    }


startsWith : Common.Callable io
startsWith =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.startsWith substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: starts-with? expects a string")
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "substr" ) <| Callable.toArityFunction arity2
        , doc = Just "True if s starts with substr."
    }


includes : Common.Callable io
includes =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.contains substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: includes? expects two string arguments")
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "substr" ) <| Callable.toArityFunction arity2
        , doc = Just "True if s includes substr."
    }


indexOf : Common.Callable io
indexOf =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2
                (\s substr ->
                    String.indexes substr s
                        |> List.head
                        |> Maybe.map Value.int
                        |> Maybe.withDefault Nil
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: index-of expects two string arguments")

        arity3 ( sValue, substrValue, fromIndexValue ) =
            Maybe.map3
                (\s substr fromIndex ->
                    String.indexes substr s
                        |> List.filter (\i -> i >= fromIndex)
                        |> List.head
                        |> Maybe.map Value.int
                        |> Maybe.withDefault Nil
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryString substrValue)
                (Value.tryInt fromIndexValue)
                |> Result.fromMaybe (Value.exception "type error: last-index-of expects two string arguments and one int argument")
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "substr" ) <| Callable.toArityFunction arity2
        , arity3 = Just <| Fixed ( Symbol "s", Symbol "substr", Symbol "from-index" ) <| Callable.toArityFunction arity3
        , doc = Just "Return index of value (string or char) in s, optionally searching forward from from-index. Return nil if value not found."
    }


lastIndexOf : Common.Callable io
lastIndexOf =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2
                (\s substr ->
                    String.indexes substr s
                        |> List.reverse
                        |> List.head
                        |> Maybe.map Value.int
                        |> Maybe.withDefault Nil
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: index-of expects two string arguments")

        arity3 ( sValue, substrValue, fromIndexValue ) =
            Maybe.map3
                (\s substr fromIndex ->
                    String.indexes substr s
                        |> List.filter (\i -> i <= fromIndex)
                        |> List.reverse
                        |> List.head
                        |> Maybe.map Value.int
                        |> Maybe.withDefault Nil
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryString substrValue)
                (Value.tryInt fromIndexValue)
                |> Result.fromMaybe (Value.exception "type error: last-index-of expects two string arguments and one int argument")
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "substr" ) <| Callable.toArityFunction arity2
        , arity3 = Just <| Fixed ( Symbol "s", Symbol "substr", Symbol "from-index" ) <| Callable.toArityFunction arity3
        , doc = Just "Return last index of value (string or char) in s, optionally searching backward from from-index. Return nil if value not found."
    }


lowerCase : Common.Callable io
lowerCase =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.toLower >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: lower-case expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Converts string to all lower-case."
    }


upperCase : Common.Callable io
upperCase =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.toUpper >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: upper-case expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Converts string to all upper-case."
    }


replaceMatch : String -> Regex.Match -> String
replaceMatch pattern match =
    match.submatches
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( i, mSubmatch ) a ->
                case mSubmatch of
                    Just submatch ->
                        String.replace ("$" ++ String.fromInt (i + 1)) submatch a

                    Nothing ->
                        a
            )
            pattern


replace : Common.Callable io
replace =
    let
        arity3 ( sValue, matchValue, replacementValue ) =
            Maybe.map3
                (\s replaceMatchFn replacement ->
                    replaceMatchFn replacement s
                        |> String
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryOneOf
                    [ Value.tryString >> Maybe.map String.replace
                    , Value.tryRegex >> Maybe.map (\regex -> \replacement -> Regex.replace regex (replaceMatch replacement))
                    ]
                    matchValue
                )
                (Value.tryString replacementValue)
                |> Result.fromMaybe (Value.exception "type error: wrong argument types to replace")
    in
    { emptyCallable
        | arity3 = Just <| Fixed ( Symbol "s", Symbol "match", Symbol "replacement" ) <| Callable.toArityFunction arity3
        , doc = Just """Replaces all instance of match with replacement in s.

match/replacement can be:

string / string
pattern / string

See also replace-first.

The replacement is literal (i.e. none of its characters are treated
specially) for all cases above except pattern / string.

For pattern / string, $1, $2, etc. in the replacement string are
substituted with the string that matched the corresponding
parenthesized group in the pattern.  If you wish your replacement
string r to be used literally, use (re-quote-replacement r) as the
replacement argument.  See also documentation for
java.util.regex.Matcher's appendReplacement method.

Example:
(string/replace "Almost Pig Latin" #"\\b(\\w)(\\w+)\\b" "$2$1ay")
-> "lmostAay igPay atinLay" """
    }


replaceFirst : Common.Callable io
replaceFirst =
    let
        arity3 ( sValue, matchValue, replacementValue ) =
            Maybe.map3
                (\s replaceMatchFn replacement ->
                    replaceMatchFn replacement s
                        |> String
                        |> Const
                )
                (Value.tryString sValue)
                (Value.tryOneOf
                    [ Value.tryString >> Maybe.map String.replace
                    , Value.tryRegex >> Maybe.map (\regex -> \replacement -> Regex.replaceAtMost 1 regex (replaceMatch replacement))
                    ]
                    matchValue
                )
                (Value.tryString replacementValue)
                |> Result.fromMaybe (Value.exception "type error: wrong argument types to replace-first")
    in
    { emptyCallable
        | arity3 = Just <| Fixed ( Symbol "s", Symbol "match", Symbol "replacement" ) <| Callable.toArityFunction arity3
        , doc = Just """Usage: (replace s match replacement)
Replaces all instance of match with replacement in s.

match/replacement can be:

string / string
pattern / string

See also replace-first.

The replacement is literal (i.e. none of its characters are treated
specially) for all cases above except pattern / string.

For pattern / string, $1, $2, etc. in the replacement string are
substituted with the string that matched the corresponding
parenthesized group in the pattern.  If you wish your replacement
string r to be used literally, use (re-quote-replacement r) as the
replacement argument.  See also documentation for
java.util.regex.Matcher's appendReplacement method.

Example:
(string/replace "Almost Pig Latin" #"\\b(\\w)(\\w+)\\b" "$2$1ay")
-> "lmostAay igPay atinLay" """
    }


reverse : Common.Callable io
reverse =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.reverse >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: reverse expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Returns s with its characters reversed."
    }


split : Common.Callable io
split =
    let
        arity2 ( sValue, splitstrValue ) =
            Result.map2 (\s splitFn -> splitFn s |> List.map String |> Value.vectorFromList |> Const)
                (Value.tryString sValue |> Result.fromMaybe (Value.exception "type error: the first argument to split should be a string"))
                (Value.tryOneOf
                    [ Value.tryString >> Maybe.map String.split
                    , Value.tryRegex >> Maybe.map Regex.split
                    ]
                    splitstrValue
                    |> Result.fromMaybe (Value.exception "type error: the second argument to split should be a string or a regular expression")
                )
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "s", Symbol "splitter" ) <| Callable.toArityFunction arity2
        , doc = Just """Splits string on a regular expression or a string.  Optional argument limit is
the maximum number of parts. Not lazy. Returns vector of the parts.
Trailing empty strings are not returned - pass limit of -1 to return all."""
    }


trim : Common.Callable io
trim =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trim >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: trim expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Removes whitespace from both ends of string."
    }


triml : Common.Callable io
triml =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trimLeft >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: triml expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Removes whitespace from the left side of string."
    }


trimr : Common.Callable io
trimr =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trimRight >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: trimr expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| Callable.toArityFunction arity1
        , doc = Just "Removes whitespace from the right side of string."
    }
