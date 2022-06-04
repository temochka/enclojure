module Enclojure.ValueSet exposing (ValueSet, empty, fromList, insert, isEmpty, map, member, remove, toList)

import Enclojure.Common exposing (Number(..), Value(..), areEqualValues)
import Set


type alias ValueSet io =
    Enclojure.Common.ValueSet io


empty : ValueSet io
empty =
    Enclojure.Common.ValueSet
        { ints = Set.empty
        , floats = Set.empty
        , strings = Set.empty
        , nil = False
        , true = False
        , false = False
        , symbols = Set.empty
        , keywords = Set.empty
        , otherValues = []
        }


isEmpty : ValueSet io -> Bool
isEmpty (Enclojure.Common.ValueSet m) =
    Set.isEmpty m.ints
        && Set.isEmpty m.floats
        && Set.isEmpty m.strings
        && List.isEmpty m.otherValues
        && m.nil
        == False
        && m.true
        == False
        && m.false
        == False
        && Set.isEmpty m.symbols
        && Set.isEmpty m.keywords


insertOtherValue : Value io -> List (Value io) -> List (Value io)
insertOtherValue v list =
    case list of
        existingValue :: rst ->
            if areEqualValues existingValue v then
                existingValue :: rst

            else
                existingValue :: insertOtherValue v rst

        [] ->
            [ v ]


insert : Value io -> ValueSet io -> ValueSet io
insert v (Enclojure.Common.ValueSet set) =
    Enclojure.Common.ValueSet <|
        case v of
            Number (Int int) ->
                { set | ints = Set.insert int set.ints }

            Number (Float float) ->
                { set | floats = Set.insert float set.floats }

            String string ->
                { set | strings = Set.insert string set.strings }

            Nil ->
                { set | nil = True }

            Bool True ->
                { set | true = True }

            Bool False ->
                { set | false = True }

            Keyword keyword ->
                { set | keywords = Set.insert keyword set.keywords }

            Symbol symbol ->
                { set | symbols = Set.insert symbol set.symbols }

            _ ->
                { set | otherValues = insertOtherValue v set.otherValues }


remove : Value io -> ValueSet io -> ValueSet io
remove v (Enclojure.Common.ValueSet set) =
    Enclojure.Common.ValueSet <|
        case v of
            Number (Int int) ->
                { set | ints = Set.remove int set.ints }

            Number (Float float) ->
                { set | floats = Set.remove float set.floats }

            String string ->
                { set | strings = Set.remove string set.strings }

            Nil ->
                { set | nil = False }

            Bool True ->
                { set | true = False }

            Bool False ->
                { set | false = False }

            Keyword keyword ->
                { set | keywords = Set.remove keyword set.keywords }

            Symbol symbol ->
                { set | symbols = Set.remove symbol set.symbols }

            _ ->
                { set | otherValues = set.otherValues |> List.filter (areEqualValues v >> not) }


fromList : List (Value io) -> ValueSet io
fromList entries =
    entries
        |> List.foldl (\v a -> insert v a) empty


toList : ValueSet io -> List (Value io)
toList (Enclojure.Common.ValueSet set) =
    let
        ints =
            Set.toList set.ints |> List.map (Int >> Number)

        floats =
            Set.toList set.floats |> List.map (Float >> Number)

        strings =
            Set.toList set.strings |> List.map String

        nils =
            if set.nil then
                [ Nil ]

            else
                []

        trues =
            if set.true then
                [ Bool True ]

            else
                []

        falses =
            if set.false then
                [ Bool False ]

            else
                []

        keywords =
            Set.toList set.keywords |> List.map Keyword

        symbols =
            Set.toList set.symbols |> List.map Symbol
    in
    ints
        ++ floats
        ++ strings
        ++ nils
        ++ trues
        ++ falses
        ++ keywords
        ++ symbols
        ++ set.otherValues


map : (Value io -> Value io) -> ValueSet io -> ValueSet io
map f set =
    set |> toList |> List.map f |> fromList


member : Value io -> ValueSet io -> Bool
member v (Enclojure.Common.ValueSet set) =
    case v of
        Number (Int int) ->
            Set.member int set.ints

        Number (Float float) ->
            Set.member float set.floats

        String string ->
            Set.member string set.strings

        Nil ->
            set.nil

        Bool False ->
            set.false

        Bool True ->
            set.true

        Keyword keyword ->
            Set.member keyword set.keywords

        Symbol symbol ->
            Set.member symbol set.symbols

        _ ->
            List.member v set.otherValues
