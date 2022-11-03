module Enclojure.ValueKeyMap exposing
    ( ValueKeyMap, ValueKeyMapEntry, empty, fromList
    , get, keys, values, toList
    , foldl, insert, map, remove
    , isEmpty, member
    )

{-| Represents a dictionary-like type where the keys are Enclojure values and values are arbitrary Elm types.
Used internally by `ValueMap io`.


# Creating

@docs ValueKeyMap, ValueKeyMapEntry, empty, fromList


# Accessing values

@docs get, keys, values, toList


# Modifying

@docs foldl, insert, map, remove


# Predicates

@docs isEmpty, member

-}

import Dict
import Enclojure.Common exposing (Number(..), Value(..), ValueMap(..), ValueMapEntry, areEqualValues, linearFind)


{-| Represents a map of values to `v`. Operation complexity depends on the type of the key.
For keywords, symbols, strings, floats, and integers, the complexity of insert/remove operations is logarithmic.
For other value types, the complexity ranges from linear or worse, depending on the type of key values.
-}
type alias ValueKeyMap io v =
    Enclojure.Common.ValueMap io v


{-| Represents a map entry.
-}
type alias ValueKeyMapEntry io v =
    Enclojure.Common.ValueMapEntry io v


{-| Returns an empty map.
-}
empty : ValueKeyMap io v
empty =
    ValueMap
        { ints = Dict.empty
        , floats = Dict.empty
        , strings = Dict.empty
        , nil = Nothing
        , true = Nothing
        , false = Nothing
        , symbols = Dict.empty
        , keywords = Dict.empty
        , otherValues = []
        }


{-| Returns True if the map is empty.
-}
isEmpty : ValueKeyMap io v -> Bool
isEmpty (ValueMap m) =
    Dict.isEmpty m.ints
        && Dict.isEmpty m.floats
        && Dict.isEmpty m.strings
        && m.nil
        == Nothing
        && m.true
        == Nothing
        && m.false
        == Nothing
        && Dict.isEmpty m.symbols
        && List.isEmpty m.otherValues
        && Dict.isEmpty m.keywords


insertOtherValue : Value io -> v -> List (ValueKeyMapEntry io v) -> List (ValueKeyMapEntry io v)
insertOtherValue k v list =
    case list of
        (( existingKey, _ ) as entry) :: rst ->
            if areEqualValues existingKey k then
                ( k, v ) :: rst

            else
                entry :: insertOtherValue k v rst

        [] ->
            [ ( k, v ) ]


{-| Inserts a value specified by the second argument to the key specified by the first argument.
-}
insert : Value io -> v -> ValueKeyMap io v -> ValueMap io v
insert k v (ValueMap m) =
    (case k of
        Number (Int int) ->
            { m | ints = Dict.insert int v m.ints }

        Number (Float float) ->
            { m | floats = Dict.insert float v m.floats }

        String string ->
            { m | strings = Dict.insert string v m.strings }

        Nil ->
            { m | nil = Just v }

        Bool True ->
            { m | true = Just v }

        Bool False ->
            { m | false = Just v }

        Keyword keyword ->
            { m | keywords = Dict.insert keyword v m.keywords }

        Symbol symbol ->
            { m | symbols = Dict.insert symbol v m.symbols }

        _ ->
            { m | otherValues = insertOtherValue k v m.otherValues }
    )
        |> ValueMap


{-| Removes the value at a given key from the map.
-}
remove : Value io -> ValueKeyMap io v -> ValueKeyMap io v
remove k (ValueMap m) =
    (case k of
        Number (Int int) ->
            { m | ints = Dict.remove int m.ints }

        Number (Float float) ->
            { m | floats = Dict.remove float m.floats }

        String string ->
            { m | strings = Dict.remove string m.strings }

        Nil ->
            { m | nil = Nothing }

        Bool True ->
            { m | true = Nothing }

        Bool False ->
            { m | false = Nothing }

        Keyword keyword ->
            { m | keywords = Dict.remove keyword m.keywords }

        Symbol symbol ->
            { m | symbols = Dict.remove symbol m.symbols }

        _ ->
            { m | otherValues = m.otherValues |> List.filter (Tuple.first >> areEqualValues k >> not) }
    )
        |> ValueMap


{-| Returns the value at a given key in the map if present.
-}
get : Value io -> ValueKeyMap io v -> Maybe v
get k (ValueMap m) =
    case k of
        Number (Int int) ->
            Dict.get int m.ints

        Number (Float float) ->
            Dict.get float m.floats

        String string ->
            Dict.get string m.strings

        Nil ->
            m.nil

        Bool True ->
            m.true

        Bool False ->
            m.false

        Keyword keyword ->
            Dict.get keyword m.keywords

        Symbol symbol ->
            Dict.get symbol m.symbols

        _ ->
            linearFind (Tuple.first >> areEqualValues k) m.otherValues
                |> Maybe.map Tuple.second


{-| Returns True if the map has a value at a given key.
-}
member : Value io -> ValueKeyMap io v -> Bool
member keyVal m =
    Nothing /= get keyVal m


{-| Transforms a given map into a list of map entries.
-}
toList : ValueKeyMap io v -> List (ValueKeyMapEntry io v)
toList (ValueMap m) =
    let
        ints =
            Dict.toList m.ints |> List.map (Tuple.mapFirst (Int >> Number))

        floats =
            Dict.toList m.floats |> List.map (Tuple.mapFirst (Float >> Number))

        strings =
            Dict.toList m.strings |> List.map (Tuple.mapFirst String)

        nils =
            m.nil |> Maybe.map (Tuple.pair Nil >> List.singleton) |> Maybe.withDefault []

        trues =
            m.true |> Maybe.map (Tuple.pair (Bool True) >> List.singleton) |> Maybe.withDefault []

        falses =
            m.false |> Maybe.map (Tuple.pair (Bool False) >> List.singleton) |> Maybe.withDefault []

        keywords =
            Dict.toList m.keywords |> List.map (Tuple.mapFirst Keyword)

        symbols =
            Dict.toList m.symbols |> List.map (Tuple.mapFirst Symbol)
    in
    ints
        ++ floats
        ++ strings
        ++ nils
        ++ trues
        ++ falses
        ++ keywords
        ++ symbols
        ++ m.otherValues


{-| Folds a given map from left to right using a function that accepts the key, the value, and the accumulator,
and is called for each entry in the map.
-}
foldl : (Value io -> v -> a -> a) -> a -> ValueKeyMap io v -> a
foldl fn init m =
    List.foldl (\( k, v ) a -> fn k v a) init (toList m)


{-| Creates a map from a given list of map entries.
-}
fromList : List (ValueKeyMapEntry io v) -> ValueKeyMap io v
fromList entries =
    entries
        |> List.foldl (\( k, v ) a -> insert k v a) empty


{-| Applies a function to every mapEntry in the map.
-}
map : (ValueKeyMapEntry io v -> ValueKeyMapEntry io v) -> ValueKeyMap io v -> ValueKeyMap io v
map f m =
    m |> toList |> List.map f |> fromList


{-| Returns the list of map values
-}
values : ValueKeyMap io v -> List v
values m =
    m |> toList |> List.map Tuple.second


{-| Returns the list of map keys.
-}
keys : ValueKeyMap io v -> List (Value io)
keys m =
    m |> toList |> List.map Tuple.first
