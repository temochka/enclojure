module Enclojure.Lib.String exposing (init)

import Enclojure.Callable as Callable
import Enclojure.Located as Located
import Enclojure.Runtime as Runtime
import Enclojure.Common as Types exposing (Arity(..), Callable, Exception(..), IO(..), Value(..))
import Enclojure.Value as Value exposing (inspect)
import Regex


emptyCallable : Callable io
emptyCallable =
    Callable.new


init : Types.Env io -> Types.Env io
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
                Runtime.bindGlobal name (Fn (Just name) (Callable.toThunk fn))
            )
            env


splitLines : Types.Callable io
splitLines =
    let
        arity1 val =
            case val of
                String s ->
                    String.lines s
                        |> List.map (String >> Located.unknown)
                        |> Types.List
                        |> Ok

                _ ->
                    Err (Exception ("type error: expected string, got " ++ inspect val) [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
    }


length : Types.Callable io
length =
    let
        arity1 val =
            Value.tryString val
                |> Maybe.map (String.length >> Types.Int >> Types.Number >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected string, got " ++ inspect val) []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
    }


join : Types.Callable io
join =
    let
        arity1 val =
            val
                |> Value.trySequenceOf (Value.toString >> Just)
                |> Maybe.map (String.join "" >> Types.String >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected a sequence, got " ++ inspect val) []))

        arity2 ( sepVal, collVal ) =
            Maybe.map2 (\sep coll -> String.join sep coll |> Types.String |> Ok)
                (Value.tryString sepVal)
                (Value.trySequenceOf (Value.toString >> Just) collVal)
                |> Maybe.withDefault (Err (Exception "type error: expected a separator and a sequence of strings" []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
        , arity2 = Just <| Fixed <| Callable.toArityFunction (arity2 >> Result.map Const)
    }


isBlank : Types.Callable io
isBlank =
    let
        arity1 s =
            s
                |> Value.tryString
                |> Maybe.map (String.trim >> String.isEmpty >> Bool >> Const)
                |> Result.fromMaybe (Value.exception "type error: blank? expects a string")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


capitalize : Types.Callable io
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
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


endsWith : Types.Callable io
endsWith =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.endsWith substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: ends-with? expects a string")
    in
    { emptyCallable
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
    }


startsWith : Types.Callable io
startsWith =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.startsWith substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: starts-with? expects a string")
    in
    { emptyCallable
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
    }


includes : Types.Callable io
includes =
    let
        arity2 ( sValue, substrValue ) =
            Maybe.map2 (\s substr -> String.contains substr s |> Bool |> Const)
                (Value.tryString sValue)
                (Value.tryString substrValue)
                |> Result.fromMaybe (Value.exception "type error: includes? expects two string arguments")
    in
    { emptyCallable
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
    }


indexOf : Types.Callable io
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
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
        , arity3 = Just <| Fixed <| Callable.toArityFunction arity3
    }


lastIndexOf : Types.Callable io
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
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
        , arity3 = Just <| Fixed <| Callable.toArityFunction arity3
    }


lowerCase : Types.Callable io
lowerCase =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.toLower >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: lower-case expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


upperCase : Types.Callable io
upperCase =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.toUpper >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: upper-case expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
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


replace : Types.Callable io
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
        | arity3 = Just <| Fixed <| Callable.toArityFunction arity3
    }


replaceFirst : Types.Callable io
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
        | arity3 = Just <| Fixed <| Callable.toArityFunction arity3
    }


reverse : Types.Callable io
reverse =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.reverse >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: reverse expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


split : Types.Callable io
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
        | arity2 = Just <| Fixed <| Callable.toArityFunction arity2
    }


trim : Types.Callable io
trim =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trim >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: trim expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


triml : Types.Callable io
triml =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trimLeft >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: triml expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }


trimr : Types.Callable io
trimr =
    let
        arity1 sValue =
            sValue
                |> Value.tryString
                |> Maybe.map (String.trimRight >> String >> Const)
                |> Result.fromMaybe (Value.exception "type error: trimr expects one string argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction arity1
    }
