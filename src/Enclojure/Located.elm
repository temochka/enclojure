module Enclojure.Located exposing
    ( Located(..), Location(..), Offsets
    , at, getValue, map, sameAs, unknown
    , getLocation, getOffsets
    )

{-| Some values have known locations in the evaluated source code. This module provides functions for
working with them.


# Types

@docs Located, Location, Offsets


# Working with located values

@docs at, getValue, map, sameAs, unknown


# Extracting location information

@docs getLocation, getOffsets

-}


{-| Represents the offsets of the start and the end of a value in the source code.
-}
type alias Offsets =
    { start : ( Int, Int ), end : ( Int, Int ) }


{-| Represents the location of a value in the source code. Known offsets or unknown.
-}
type Location
    = Known Offsets
    | Unknown


{-| Represents a value `a` with location information.
-}
type Located a
    = Located Location a


{-| Assigns location information from `a` to `b`
-}
sameAs : Located a -> b -> Located b
sameAs (Located pos _) val =
    Located pos val


{-| Applies a function to the located value
-}
map : (a -> b) -> Located a -> Located b
map f (Located pos a) =
    Located pos (f a)


{-| Extracts the located value
-}
getValue : Located a -> a
getValue (Located _ val) =
    val


{-| Extract the offsets of the located value (if known)
-}
getOffsets : Located a -> Maybe Offsets
getOffsets (Located location _) =
    case location of
        Known offsets ->
            Just offsets

        Unknown ->
            Nothing


{-| Extract the location of a located value
-}
getLocation : Located a -> Location
getLocation (Located location _) =
    location


{-| Wraps `a` in an unknown location
-}
unknown : a -> Located a
unknown v =
    Located Unknown v


{-| Wraps `a` in a location at given offsets
-}
at : ( Int, Int ) -> ( Int, Int ) -> a -> Located a
at start end val =
    Located (Known { start = start, end = end }) val
