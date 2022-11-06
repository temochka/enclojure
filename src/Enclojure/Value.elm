module Enclojure.Value exposing
    ( Value
    , inspect, inspectLocated, inspectType, isEqual, isTruthy
    , toMap, toSeq, toString
    , boolean, exception, float, fn, int, keyword, list, map, nil, string, symbol, throwable, vector, vectorFromList, vectorFromLocatedList
    , tryAtom, tryBool, tryDictOf, tryFloat, tryInt, tryKeyword, tryList, tryListOf, tryMap, tryNil, tryOneOf, tryPatternOf2, tryRef, tryRegex, trySequenceOf, tryString, trySymbol, tryVector, tryVectorOf
    )

{-| Functions for working with Enclojure values and translating between Elm and Enclojure types.


# Value type

@docs Value


# Inspecting values

@docs inspect, inspectLocated, inspectType, isEqual, isTruthy


# Transforming values

@docs toMap, toSeq, toString


# Encoding values

@docs boolean, exception, float, fn, int, keyword, list, map, nil, string, symbol, throwable, vector, vectorFromList, vectorFromLocatedList


# Decoding values

@docs tryAtom, tryBool, tryDictOf, tryFloat, tryInt, tryKeyword, tryList, tryListOf, tryMap, tryNil, tryOneOf, tryPatternOf2, tryRef, tryRegex, trySequenceOf, tryString, trySymbol, tryVector, tryVectorOf

-}

import Array
import Dict
import Enclojure.Common as Common exposing (Callable, Exception(..), Number(..), Ref(..), Value(..))
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet
import Regex exposing (Regex)


{-| Represents an Enclojure value.
-}
type alias Value io =
    Common.Value io


{-| Attempts to interpret a given value as a sequence (list).
-}
toSeq : Value io -> Result Exception (List (Located (Value io)))
toSeq val =
    case val of
        List l ->
            Ok l

        Vector v ->
            Ok <| Array.toList v

        Set s ->
            Ok <| List.map Located.unknown <| ValueSet.toList s

        Map m ->
            Ok <| List.map (\(( _, Located loc _ ) as entry) -> Located loc (MapEntry entry)) (ValueMap.toList m)

        String s ->
            Ok <| List.map (String.fromChar >> String >> Located.unknown) (String.toList s)

        MapEntry ( k, v ) ->
            Ok <| [ Located.sameAs v k, v ]

        Nil ->
            Ok []

        _ ->
            Err <| Exception (inspect val ++ " is not a sequence") []


{-| Attempts to interpret a given value as a map.
-}
toMap : Value io -> Maybe (ValueMap io)
toMap val =
    case val of
        Nil ->
            Just ValueMap.empty

        Vector vec ->
            Array.toList vec |> List.indexedMap (\i e -> ( Number <| Int i, e )) |> ValueMap.fromList |> Just

        Map m ->
            Just m

        _ ->
            Nothing


{-| Returns a string if the given value is a string.
-}
tryString : Value io -> Maybe String
tryString value =
    case value of
        String s ->
            Just s

        _ ->
            Nothing


{-| Returns a bool if the given value is a string.
-}
tryBool : Value io -> Maybe Bool
tryBool value =
    case value of
        Bool s ->
            Just s

        _ ->
            Nothing


{-| Returns a ref if the given value is a ref.
-}
tryRef : Value io -> Maybe (Ref io)
tryRef value =
    case value of
        Ref ref ->
            Just ref

        _ ->
            Nothing


{-| Returns an atom id if the given value is an atom ref.
-}
tryAtom : Value io -> Maybe Int
tryAtom value =
    case value of
        Ref (Atom id) ->
            Just id

        _ ->
            Nothing


{-| Returns a regex if the given value is a regex.
-}
tryRegex : Value io -> Maybe Regex
tryRegex value =
    case value of
        Regex _ r ->
            Just r

        _ ->
            Nothing


{-| Returns a keyword as a string if the given value is a keyword.
-}
tryKeyword : Value io -> Maybe String
tryKeyword value =
    case value of
        Keyword s ->
            Just s

        _ ->
            Nothing


{-| Returns a symbol as a string if the given value is a symbol.
-}
trySymbol : Value io -> Maybe String
trySymbol value =
    case value of
        Symbol s ->
            Just s

        _ ->
            Nothing


{-| Returns a `ValueMap io` if the given value is a map.
-}
tryMap : Value io -> Maybe (ValueMap io)
tryMap value =
    case value of
        Map s ->
            Just s

        _ ->
            Nothing


{-| Returns a float if the given value is a float.
-}
tryFloat : Value io -> Maybe Float
tryFloat value =
    case value of
        Number (Float n) ->
            Just n

        _ ->
            Nothing


{-| Returns an integer if the given value is an integer.
-}
tryInt : Value io -> Maybe Int
tryInt value =
    case value of
        Number (Int n) ->
            Just n

        _ ->
            Nothing


{-| Returns an empty tuple if the given value is nil.
-}
tryNil : Value io -> Maybe ()
tryNil value =
    case value of
        Nil ->
            Just ()

        _ ->
            Nothing


{-| If the value is a map, attempts to convert it to an Elm dictionary using the first argument to convert values to
keys and the second argument to convert values.
-}
tryDictOf : (Value io -> Maybe comparable) -> (Value io -> Maybe b) -> Value io -> Maybe (Dict.Dict comparable b)
tryDictOf extractKey extractValue value =
    let
        extractAllKv kvSequence =
            kvSequence
                |> List.foldr
                    (\( key, val ) a ->
                        a
                            |> Maybe.andThen
                                (\acc ->
                                    Maybe.map2
                                        (\extractedKey extractedVal -> ( extractedKey, extractedVal ) :: acc)
                                        (extractKey key)
                                        (extractValue (Located.getValue val))
                                )
                    )
                    (Just [])
    in
    case value of
        Map m ->
            m |> ValueMap.toList |> extractAllKv |> Maybe.map Dict.fromList

        _ ->
            Nothing


extractAll : (Value io -> Maybe a) -> List (Value io) -> Maybe (List a)
extractAll extract sequence =
    sequence
        |> List.foldr
            (\e a ->
                a
                    |> Maybe.andThen
                        (\acc ->
                            extract e
                                |> Maybe.map (\extracted -> extracted :: acc)
                        )
            )
            (Just [])


{-| Returns an array of values if the given value is a vector.
-}
tryVector : Value io -> Maybe (Array.Array (Located (Value io)))
tryVector value =
    case value of
        Vector v ->
            Just v

        _ ->
            Nothing


{-| If the given value is a vector, returns a list of `a` using the first argument as a function to interpret vector
values as `a`.
-}
tryVectorOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
tryVectorOf extract value =
    case value of
        Vector v ->
            Array.toList v
                |> List.map Located.getValue
                |> extractAll extract

        _ ->
            Nothing


{-| Returns a list of values if the given value is a list
-}
tryList : Value io -> Maybe (List (Located (Value io)))
tryList value =
    case value of
        List l ->
            Just l

        _ ->
            Nothing


{-| If the given value is a list, returns a list of `a` using the first argument as a function to interpret list
values as `a`.
-}
tryListOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
tryListOf extract value =
    case value of
        List l ->
            l
                |> List.map Located.getValue
                |> extractAll extract

        _ ->
            Nothing


{-| If the given value can be interpreted as a sequence, returns a list of `a` using the first argument as a function
to interpret list values as `a`.
-}
trySequenceOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
trySequenceOf extract value =
    toSeq value
        |> Result.map (List.map Located.getValue)
        |> Result.toMaybe
        |> Maybe.andThen (extractAll extract)


{-| Attempts to interpret the given value as `a` as one of the given "decoders".
-}
tryOneOf : List (Value io -> Maybe a) -> Value io -> Maybe a
tryOneOf decoders value =
    case decoders of
        [] ->
            Nothing

        decoder :: rest ->
            case decoder value of
                Just v ->
                    Just v

                Nothing ->
                    tryOneOf rest value


{-| Attempts to interpret a list of values as a pattern of two values of known type and the rest.
-}
tryPatternOf2 : (a -> b -> List (Value io) -> Maybe c) -> (Value io -> Maybe a) -> (Value io -> Maybe b) -> List (Value io) -> Maybe c
tryPatternOf2 combine matchA matchB values =
    case values of
        a :: b :: rest ->
            Maybe.map2
                (\matchedA matchedB ->
                    combine matchedA matchedB rest
                )
                (matchA a)
                (matchB b)
                |> Maybe.andThen identity

        _ ->
            Nothing


{-| Creates an exception with a given message.
-}
exception : String -> Exception
exception message =
    Exception message []


{-| Prints a value with location information.
-}
inspectLocated : Located (Value io) -> String
inspectLocated locatedValue =
    let
        suffix =
            locatedValue
                |> Located.getOffsets
                |> Maybe.map (\{ start } -> ":" ++ String.fromInt (Tuple.first start) ++ ":" ++ String.fromInt (Tuple.second start))
                |> Maybe.withDefault ""
    in
    inspect (Located.getValue locatedValue) ++ suffix


{-| Print value in a human readable way.
-}
inspect : Value io -> String
inspect value =
    case value of
        Ref ref ->
            case ref of
                Atom id ->
                    "<atom " ++ String.fromInt id ++ ">"

                Var name _ ->
                    "#'" ++ name

        String s ->
            "\"" ++ s ++ "\""

        Number (Int x) ->
            String.fromInt x

        Number (Float x) ->
            String.fromFloat x

        Fn { name } _ ->
            "fn<" ++ (name |> Maybe.withDefault "anonymous") ++ ">"

        List l ->
            "(" ++ (List.map (Located.getValue >> inspect) l |> String.join " ") ++ ")"

        Nil ->
            "nil"

        Bool b ->
            if b then
                "true"

            else
                "false"

        Vector l ->
            "[" ++ (List.map (Located.getValue >> inspect) (Array.toList l) |> String.join " ") ++ "]"

        Keyword name ->
            ":" ++ name

        Map m ->
            List.map (\( k, Located _ v ) -> inspect k ++ " " ++ inspect v) (ValueMap.toList m)
                |> String.join ", "
                |> (\r -> "{" ++ r ++ "}")

        MapEntry ( k, v ) ->
            inspect (Vector (Array.fromList [ Located.unknown k, v ]))

        Regex s _ ->
            "#" ++ s

        Set set ->
            List.map (\v -> inspect v) (ValueSet.toList set)
                |> String.join ", "
                |> (\r -> "#{" ++ r ++ "}")

        Symbol name ->
            name

        Throwable (Exception str _) ->
            "Exception: " ++ str


{-| Prints a value as a string.
-}
print : Value io -> String
print value =
    case value of
        String s ->
            s

        Nil ->
            ""

        _ ->
            inspect value


{-| Attempts to intrpret a value as a string.
-}
toString : Value io -> String
toString value =
    case value of
        String s ->
            s

        _ ->
            print value


{-| Wraps a float as a number value.
-}
float : Float -> Value io
float n =
    Number <| Float n


{-| Wraps an int as a number value.
-}
int : Int -> Value io
int n =
    Number <| Int n


{-| Wraps a string as a string value.
-}
string : String -> Value io
string =
    String


{-| Wraps a string as a keyword value.
-}
keyword : String -> Value io
keyword =
    Keyword


{-| Wraps a string as a symbol value.
-}
symbol : String -> Value io
symbol =
    Symbol


{-| Returns a nil value.
-}
nil : Value io
nil =
    Nil


{-| Returns a boolean value for a given Elm Bool.
-}
boolean : Bool -> Value io
boolean =
    Bool


{-| Wraps a value map as a map value.
-}
map : ValueMap io -> Value io
map =
    Map


{-| Wraps a list as a list value.
-}
list : List (Value io) -> Value io
list vs =
    List <| List.map Located.unknown vs


{-| Wraps a located list as a vector value.
-}
vectorFromLocatedList : List (Located (Value io)) -> Value io
vectorFromLocatedList ls =
    Vector <| Array.fromList ls


{-| Wraps a list as a vector value.
-}
vectorFromList : List (Value io) -> Value io
vectorFromList ls =
    Vector <| Array.fromList <| List.map Located.unknown ls


{-| Wraps an array as a vector value.
-}
vector : Array.Array (Value io) -> Value io
vector =
    Vector << Array.map Located.unknown


{-| Wraps an (optionally named) function as a value.
-}
fn : Maybe String -> Callable io -> Value io
fn name callable =
    Fn { name = name, doc = Nothing, signatures = [] } (Common.toThunk callable)


{-| Wraps an exception into a value.
-}
throwable : Exception -> Value io
throwable =
    Throwable


{-| Returns True if the two values are equal.
-}
isEqual : Value io -> Value io -> Bool
isEqual =
    Common.areEqualValues


{-| Return a string representation of the value type
-}
inspectType : Value io -> String
inspectType val =
    case val of
        Number n ->
            case n of
                Int _ ->
                    "Integer"

                Float _ ->
                    "Float"

        String _ ->
            "String"

        Ref r ->
            case r of
                Atom _ ->
                    "Atom"

                Var _ _ ->
                    "Var"

        Fn _ _ ->
            "Function"

        List _ ->
            "List"

        Vector _ ->
            "Vector"

        Nil ->
            "Nil"

        Bool _ ->
            "Bool"

        Keyword _ ->
            "Keyword"

        Map _ ->
            "Map"

        MapEntry _ ->
            "MapEntry"

        Regex _ _ ->
            "Regex"

        Set _ ->
            "Set"

        Symbol _ ->
            "Symbol"

        Throwable _ ->
            "Throwable"


{-| Returns True if the value is truthy.
-}
isTruthy : Value io -> Bool
isTruthy val =
    case val of
        Nil ->
            False

        Bool False ->
            False

        _ ->
            True
