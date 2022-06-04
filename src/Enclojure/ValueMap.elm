module Enclojure.ValueMap exposing
    ( ValueMap
    , ValueMapEntry
    , empty
    , foldl
    , fromList
    , get
    , insert
    , isEmpty
    , keys
    , map
    , member
    , remove
    , toList
    , values
    )

import Dict
import Enclojure.Common exposing (Number(..), Value(..), ValueMap, ValueMapEntry, areEqualValues, linearFind)
import Enclojure.Located as Located exposing (Located(..))


type alias ValueMap io =
    Enclojure.Common.ValueMap io


type alias ValueMapEntry io =
    Enclojure.Common.ValueMapEntry io


empty : ValueMap io
empty =
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


isEmpty : ValueMap io -> Bool
isEmpty m =
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


insertOtherValue : Value io -> Located (Value io) -> List (ValueMapEntry io) -> List (ValueMapEntry io)
insertOtherValue k v list =
    case list of
        (( existingKey, _ ) as entry) :: rst ->
            if areEqualValues existingKey k then
                ( k, v ) :: rst

            else
                entry :: insertOtherValue k v rst

        [] ->
            [ ( k, v ) ]


insert : Value io -> Located (Value io) -> ValueMap io -> ValueMap io
insert k v m =
    case k of
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


remove : Value io -> ValueMap io -> ValueMap io
remove k m =
    case k of
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


get : Value io -> ValueMap io -> Maybe (Located (Value io))
get k m =
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


member : Value io -> ValueMap io -> Bool
member val m =
    Nothing /= get val m


toList : ValueMap io -> List ( Value io, Located (Value io) )
toList m =
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


foldl : (Value io -> Located (Value io) -> a -> a) -> a -> ValueMap io -> a
foldl fn init m =
    List.foldl (\( k, v ) a -> fn k v a) init (toList m)


fromList : List (ValueMapEntry io) -> ValueMap io
fromList entries =
    entries
        |> List.foldl (\( k, v ) a -> insert k v a) empty


map : (ValueMapEntry io -> ValueMapEntry io) -> ValueMap io -> ValueMap io
map f m =
    m |> toList |> List.map f |> fromList


values : ValueMap io -> List (Value io)
values m =
    m |> toList |> List.map (Tuple.second >> Located.getValue)


keys : ValueMap io -> List (Value io)
keys m =
    m |> toList |> List.map Tuple.first
