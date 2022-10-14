module Enclojure.Json exposing
    ( decodeFromString, encodeToString
    , decodeValue, encodeValue
    )

{-| Utilities for converting between JSON and Enclojure values.


# Working with JSON strings

@docs decodeFromString, encodeToString


# Working with JSON values

@docs decodeValue, encodeValue

-}

import Array
import Dict exposing (Dict)
import Enclojure.Common exposing (Exception(..), Number(..), Ref(..), Value(..))
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet
import Json.Decode
import Json.Encode


{-| A JSON decoder for Enclojure values.
-}
decodeValue : Json.Decode.Decoder (Value io)
decodeValue =
    Json.Decode.oneOf
        [ Json.Decode.string |> Json.Decode.map String
        , Json.Decode.bool |> Json.Decode.map Bool
        , Json.Decode.int |> Json.Decode.map (Int >> Number)
        , Json.Decode.float |> Json.Decode.map (Float >> Number)
        , Json.Decode.null Nil
        , Json.Decode.array (Json.Decode.lazy (\_ -> decodeValue)) |> Json.Decode.map (Array.map Located.unknown >> Vector)
        , Json.Decode.dict (Json.Decode.lazy (\_ -> decodeValue))
            |> Json.Decode.map
                (Dict.toList
                    >> List.map (\( k, v ) -> ( String k, Located.unknown v ))
                    >> ValueMap.fromList
                    >> Map
                )
        ]


{-| Accepts a JSON string. Returns the result of parsing this string as an Enclojure value.
-}
decodeFromString : String -> Result Exception (Value io)
decodeFromString json =
    Json.Decode.decodeString decodeValue json
        |> Result.mapError
            (Json.Decode.errorToString
                >> String.append "JSON parsing error: "
                >> (\msg -> Exception msg [])
            )


toDict : ValueMap io -> Dict String Json.Encode.Value
toDict map =
    map
        |> ValueMap.toList
        |> List.filterMap
            (\( k, Located _ v ) ->
                (case k of
                    String s ->
                        Just s

                    Keyword s ->
                        Just s

                    _ ->
                        Nothing
                )
                    |> Maybe.map (\stringKey -> ( stringKey, encodeValue v ))
            )
        |> Dict.fromList


{-| A JSON encoder for Enclojure values.
-}
encodeValue : Value io -> Json.Encode.Value
encodeValue val =
    case val of
        Number n ->
            case n of
                Int i ->
                    Json.Encode.int i

                Float f ->
                    Json.Encode.float f

        String s ->
            Json.Encode.string s

        Ref ref ->
            case ref of
                Var n _ ->
                    Json.Encode.string ("#'" ++ n)

                Atom _ ->
                    Json.Encode.string "<atom>"

        Fn _ _ ->
            Json.Encode.null

        List l ->
            Json.Encode.list encodeValue (List.map Located.getValue l)

        Vector a ->
            Json.Encode.array encodeValue (Array.map Located.getValue a)

        Nil ->
            Json.Encode.null

        Bool b ->
            Json.Encode.bool b

        Keyword s ->
            Json.Encode.string s

        Map vm ->
            Json.Encode.dict identity identity (toDict vm)

        MapEntry ( k, Located _ v ) ->
            Json.Encode.list encodeValue [ k, v ]

        Regex s _ ->
            Json.Encode.string s

        Set vs ->
            Json.Encode.list encodeValue (ValueSet.toList vs)

        Symbol s ->
            Json.Encode.string s

        Throwable _ ->
            Json.Encode.null


{-| Accepts an Enclojure value, returns its JSON representation as a string.
-}
encodeToString : Value io -> String
encodeToString val =
    Json.Encode.encode 0 (encodeValue val)
