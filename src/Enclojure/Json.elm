module Enclojure.Json exposing (decodeFromString, encodeToString)

import Array
import Dict exposing (Dict)
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Common exposing (Exception(..), Number(..), Ref(..), Value(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet
import Json.Decode
import Json.Encode


decode : Json.Decode.Decoder (Value io)
decode =
    Json.Decode.oneOf
        [ Json.Decode.string |> Json.Decode.map String
        , Json.Decode.bool |> Json.Decode.map Bool
        , Json.Decode.int |> Json.Decode.map (Int >> Number)
        , Json.Decode.float |> Json.Decode.map (Float >> Number)
        , Json.Decode.null Nil
        , Json.Decode.array (Json.Decode.lazy (\_ -> decode)) |> Json.Decode.map (Array.map Located.unknown >> Vector)
        , Json.Decode.dict (Json.Decode.lazy (\_ -> decode))
            |> Json.Decode.map
                (Dict.toList
                    >> List.map (\( k, v ) -> ( String k, Located.unknown v ))
                    >> ValueMap.fromList
                    >> Map
                )
        ]


decodeFromString : String -> Result Exception (Value io)
decodeFromString json =
    Json.Decode.decodeString decode json
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
                    |> Maybe.map (\stringKey -> ( stringKey, encode v ))
            )
        |> Dict.fromList


encode : Value io -> Json.Encode.Value
encode val =
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
            Json.Encode.list encode (List.map Located.getValue l)

        Vector a ->
            Json.Encode.array encode (Array.map Located.getValue a)

        Nil ->
            Json.Encode.null

        Bool b ->
            Json.Encode.bool b

        Keyword s ->
            Json.Encode.string s

        Map vm ->
            Json.Encode.dict identity identity (toDict vm)

        MapEntry ( k, Located _ v ) ->
            Json.Encode.list encode [ k, v ]

        Regex s _ ->
            Json.Encode.string s

        Set vs ->
            Json.Encode.list encode (ValueSet.toList vs)

        Symbol s ->
            Json.Encode.string s

        Throwable _ ->
            Json.Encode.null


encodeToString : Value io -> String
encodeToString val =
    Json.Encode.encode 0 (encode val)
