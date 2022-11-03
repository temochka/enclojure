module Enclojure.ValueMap exposing
    ( ValueMap, ValueMapEntry, empty, fromList
    , get, keys, values, toList
    , foldl, insert, map, remove
    , isEmpty, member
    )

{-| All Enclojure maps are backed by `ValueMap io` type. This namespace provides functions for working with this type.


# Creating

@docs ValueMap, ValueMapEntry, empty, fromList


# Accessing values

@docs get, keys, values, toList


# Modifying

@docs foldl, insert, map, remove


# Predicates

@docs isEmpty, member

-}

import Enclojure.Common exposing (Value)
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.ValueKeyMap as ValueKeyMap exposing (ValueKeyMap, ValueKeyMapEntry)


{-| Represents a map of values to located values. Operation complexity depends on the type of the key.
For keywords, symbols, strings, floats, and integers, the complexity of insert/remove operations is logarithmic.
For other value types, the complexity ranges from linear or worse, depending on the type of key values.
-}
type alias ValueMap io =
    ValueKeyMap io (Located (Value io))


{-| Represents a map entry.
-}
type alias ValueMapEntry io =
    ValueKeyMapEntry io (Located (Value io))


{-| Returns an empty map.
-}
empty : ValueMap io
empty =
    ValueKeyMap.empty


{-| Returns True if the map is empty.
-}
isEmpty : ValueMap io -> Bool
isEmpty =
    ValueKeyMap.isEmpty


{-| Inserts a located value specified by the second argument to the key specified by the first argument.
-}
insert : Value io -> Located (Value io) -> ValueMap io -> ValueMap io
insert =
    ValueKeyMap.insert


{-| Removes the value at a given key from the map.
-}
remove : Value io -> ValueMap io -> ValueMap io
remove =
    ValueKeyMap.remove


{-| Returns the located value at a given key in the map if present.
-}
get : Value io -> ValueMap io -> Maybe (Located (Value io))
get =
    ValueKeyMap.get


{-| Returns True if the map has a value at a given key.
-}
member : Value io -> ValueMap io -> Bool
member =
    ValueKeyMap.member


{-| Transforms a given map into a list of map entries.
-}
toList : ValueMap io -> List (ValueMapEntry io)
toList =
    ValueKeyMap.toList


{-| Folds a given map from left to right using a function that accepts the key, the located value, and the accumulator,
and is called for each entry in the map.
-}
foldl : (Value io -> Located (Value io) -> a -> a) -> a -> ValueMap io -> a
foldl =
    ValueKeyMap.foldl


{-| Creates a map from a given list of map entries.
-}
fromList : List (ValueMapEntry io) -> ValueMap io
fromList =
    ValueKeyMap.fromList


{-| Applies a function to every mapEntry in the map.
-}
map : (ValueMapEntry io -> ValueMapEntry io) -> ValueMap io -> ValueMap io
map =
    ValueKeyMap.map


{-| Returns the list of map values without their source code locations.
-}
values : ValueMap io -> List (Value io)
values =
    ValueKeyMap.values >> List.map Located.getValue


{-| Returns the list of map keys.
-}
keys : ValueMap io -> List (Value io)
keys =
    ValueKeyMap.keys
