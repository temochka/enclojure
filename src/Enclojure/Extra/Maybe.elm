module Enclojure.Extra.Maybe exposing (orElse)


orElse : (() -> Maybe a) -> Maybe a -> Maybe a
orElse b a =
    case a of
        Just _ ->
            a

        Nothing ->
            b ()
