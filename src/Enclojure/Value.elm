module Enclojure.Value exposing
    ( Value
    , exception
    , float
    , fn
    , inspect
    , inspectLocated
    , inspectType
    , int
    , isEqual
    , keyword
    , list
    , map
    , nil
    , string
    , symbol
    , throwable
    , toMap
    , toSeq
    , toString
    , tryAtom
    , tryDictOf
    , tryFloat
    , tryInt
    , tryKeyword
    , tryListOf
    , tryMap
    , tryNil
    , tryOneOf
    , tryPatternOf2
    , tryRef
    , tryRegex
    , trySequenceOf
    , tryString
    , trySymbol
    , tryVectorOf
    , vector
    , vectorFromList
    , vectorFromLocatedList
    )

import Array
import Dict
import Enclojure.Callable as Callable
import Enclojure.Common exposing (Callable, Exception(..), Number(..), Ref(..), Value(..))
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet
import Regex exposing (Regex)


type alias Value io =
    Enclojure.Common.Value io


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


tryString : Value io -> Maybe String
tryString value =
    case value of
        String s ->
            Just s

        _ ->
            Nothing


tryRef : Value io -> Maybe (Ref io)
tryRef value =
    case value of
        Ref ref ->
            Just ref

        _ ->
            Nothing


tryAtom : Value io -> Maybe Int
tryAtom value =
    case value of
        Ref (Atom id) ->
            Just id

        _ ->
            Nothing


tryRegex : Value io -> Maybe Regex
tryRegex value =
    case value of
        Regex _ r ->
            Just r

        _ ->
            Nothing


tryKeyword : Value io -> Maybe String
tryKeyword value =
    case value of
        Keyword s ->
            Just s

        _ ->
            Nothing


trySymbol : Value io -> Maybe String
trySymbol value =
    case value of
        Symbol s ->
            Just s

        _ ->
            Nothing


tryMap : Value io -> Maybe (ValueMap io)
tryMap value =
    case value of
        Map s ->
            Just s

        _ ->
            Nothing


tryFloat : Value io -> Maybe Float
tryFloat value =
    case value of
        Number (Float n) ->
            Just n

        _ ->
            Nothing


tryInt : Value io -> Maybe Int
tryInt value =
    case value of
        Number (Int n) ->
            Just n

        _ ->
            Nothing


tryNil : Value io -> Maybe ()
tryNil value =
    case value of
        Nil ->
            Just ()

        _ ->
            Nothing


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


tryVectorOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
tryVectorOf extract value =
    case value of
        Vector v ->
            Array.toList v
                |> List.map Located.getValue
                |> extractAll extract

        _ ->
            Nothing


tryListOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
tryListOf extract value =
    case value of
        List l ->
            l
                |> List.map Located.getValue
                |> extractAll extract

        _ ->
            Nothing


trySequenceOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
trySequenceOf extract value =
    toSeq value
        |> Result.map (List.map Located.getValue)
        |> Result.toMaybe
        |> Maybe.andThen (extractAll extract)


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


exception : String -> Exception
exception message =
    Exception message []


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

        Fn name _ ->
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


print : Value io -> String
print value =
    case value of
        String s ->
            s

        Nil ->
            ""

        _ ->
            inspect value


toString : Value io -> String
toString value =
    case value of
        String s ->
            s

        _ ->
            print value


float : Float -> Value io
float n =
    Number <| Float n


int : Int -> Value io
int n =
    Number <| Int n


string : String -> Value io
string =
    String


keyword : String -> Value io
keyword =
    Keyword


symbol : String -> Value io
symbol =
    Symbol


nil : Value io
nil =
    Nil


map : ValueMap io -> Value io
map =
    Map


list : List (Value io) -> Value io
list vs =
    List <| List.map Located.unknown vs


vectorFromLocatedList : List (Located (Value io)) -> Value io
vectorFromLocatedList ls =
    Vector <| Array.fromList ls


vectorFromList : List (Value io) -> Value io
vectorFromList ls =
    Vector <| Array.fromList <| List.map Located.unknown ls


vector : Array.Array (Value io) -> Value io
vector =
    Vector << Array.map Located.unknown


fn : Maybe String -> Callable io -> Value io
fn name callable =
    Fn name (Callable.toThunk callable)


throwable : Exception -> Value io
throwable =
    Throwable


isEqual : Value io -> Value io -> Bool
isEqual =
    Enclojure.Common.areEqualValues


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
