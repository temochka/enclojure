module Enclojure.Located exposing (Located(..), Location(..), at, getLocation, getOffsets, getValue, map, sameAs, unknown)


type alias Offsets =
    { start : ( Int, Int ), end : ( Int, Int ) }


type Location
    = Known Offsets
    | Unknown


type Located a
    = Located Location a


sameAs : Located a -> b -> Located b
sameAs (Located pos _) val =
    Located pos val


map : (a -> b) -> Located a -> Located b
map f (Located pos a) =
    Located pos (f a)


getValue : Located a -> a
getValue (Located _ val) =
    val


getOffsets : Located a -> Maybe Offsets
getOffsets (Located location _) =
    case location of
        Known offsets ->
            Just offsets

        Unknown ->
            Nothing


getLocation : Located a -> Location
getLocation (Located location _) =
    location


unknown : a -> Located a
unknown v =
    Located Unknown v


at : ( Int, Int ) -> ( Int, Int ) -> a -> Located a
at start end val =
    Located (Known { start = start, end = end }) val
