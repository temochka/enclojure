module Enclojure.Common exposing
    ( Arity(..)
    , Callable
    , Continuation
    , Env
    , Exception(..)
    , IO(..)
    , Number(..)
    , Ref(..)
    , StackFrame
    , Step
    , Thunk(..)
    , Value(..)
    , ValueMap
    , ValueMapEntry
    , ValueSet(..)
    , areEqualValues
    , linearFind
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Enclojure.Located as Located exposing (Located(..))
import Regex exposing (Regex)
import Set


type Exception
    = Exception String (List StackFrame)


type alias Step io =
    ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) )


type alias Continuation io =
    Located (Value io) -> Env io -> Located (Step io)


type Thunk io
    = Thunk (Continuation io)


type Arity io a
    = Fixed (a -> Env io -> Continuation io -> ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) ))
    | Variadic ({ args : a, rest : List (Value io) } -> Env io -> Continuation io -> ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) ))


type IO io
    = Const (Value io)
    | SideEffect io


type alias Callable io =
    { arity0 : Maybe (Arity io ())
    , arity1 : Maybe (Arity io (Value io))
    , arity2 : Maybe (Arity io ( Value io, Value io ))
    , arity3 : Maybe (Arity io ( Value io, Value io, Value io ))
    }


type alias ValueMapEntry io =
    ( Value io, Located (Value io) )


type alias ValueMap io =
    { ints : Dict Int (Located (Value io))
    , floats : Dict Float (Located (Value io))
    , strings : Dict String (Located (Value io))
    , keywords : Dict String (Located (Value io))
    , symbols : Dict String (Located (Value io))
    , nil : Maybe (Located (Value io))
    , true : Maybe (Located (Value io))
    , false : Maybe (Located (Value io))
    , otherValues : List ( Value io, Located (Value io) )
    }


type ValueSet io
    = ValueSet
        { ints : Set.Set Int
        , floats : Set.Set Float
        , strings : Set.Set String
        , nil : Bool
        , true : Bool
        , false : Bool
        , keywords : Set.Set String
        , symbols : Set.Set String
        , otherValues : List (Value io)
        }


type Number
    = Float Float
    | Int Int


type Ref io
    = Var String (Value io)
    | Atom Int


type Value io
    = Number Number
    | String String
    | Ref (Ref io)
    | Fn (Maybe String) ({ self : Value io, k : Continuation io } -> Thunk io)
    | List (List (Located (Value io)))
    | Vector (Array (Located (Value io)))
    | Nil
    | Bool Basics.Bool
    | Keyword String
    | Map (ValueMap io)
    | MapEntry (ValueMapEntry io)
    | Regex String Regex
    | Set (ValueSet io)
    | Symbol String
    | Throwable Exception


type alias StackFrame =
    { name : String
    , location : Located.Location
    }


type alias Env io =
    { globalScope : Dict String (Value io)
    , lexicalScope : Dict String (Value io)
    , atoms : Dict Int (Value io)
    , stack : List StackFrame
    , atomIdGenerator : Int
    }


areEqualLists : List (Value io) -> List (Value io) -> Bool
areEqualLists listA listB =
    case ( listA, listB ) of
        ( headA :: restA, headB :: restB ) ->
            if areEqualValues headA headB then
                areEqualLists restA restB

            else
                False

        ( [], [] ) ->
            True

        _ ->
            False


areEqualLocatedLists : List (Located (Value io)) -> List (Located (Value io)) -> Bool
areEqualLocatedLists listA listB =
    case ( listA, listB ) of
        ( (Located _ headA) :: restA, (Located _ headB) :: restB ) ->
            if areEqualValues headA headB then
                areEqualLocatedLists restA restB

            else
                False

        ( [], [] ) ->
            True

        _ ->
            False


areEqualDictEntries : (key -> Value io) -> List ( key, Located (Value io) ) -> List ( key, Located (Value io) ) -> Bool
areEqualDictEntries f a b =
    case ( a, b ) of
        ( ( keyA, Located _ valA ) :: restA, ( keyB, Located _ valB ) :: restB ) ->
            if areEqualValues (f keyA) (f keyB) && areEqualValues valA valB then
                areEqualDictEntries f restA restB

            else
                False

        ( [], [] ) ->
            True

        _ ->
            False


areEqualMaps : ValueMap io -> ValueMap io -> Bool
areEqualMaps a b =
    a
        == b
        || (areEqualDictEntries (Int >> Number) (Dict.toList a.ints) (Dict.toList b.ints)
                && areEqualDictEntries (Float >> Number) (Dict.toList a.floats) (Dict.toList b.floats)
                && areEqualDictEntries String (Dict.toList a.strings) (Dict.toList b.strings)
                && areEqualDictEntries String (Dict.toList a.keywords) (Dict.toList b.keywords)
                && areEqualDictEntries String (Dict.toList a.symbols) (Dict.toList b.symbols)
                && (a.nil == b.nil || (Maybe.map2 areEqualValues (a.nil |> Maybe.map Located.getValue) (b.nil |> Maybe.map Located.getValue) |> Maybe.withDefault False))
                && (a.true == b.true || (Maybe.map2 areEqualValues (a.true |> Maybe.map Located.getValue) (b.true |> Maybe.map Located.getValue) |> Maybe.withDefault False))
                && (a.false == b.false || (Maybe.map2 areEqualValues (a.false |> Maybe.map Located.getValue) (b.false |> Maybe.map Located.getValue) |> Maybe.withDefault False))
                && areEqualDictEntries identity a.otherValues b.otherValues
           )


areEqualSets : ValueSet io -> ValueSet io -> Bool
areEqualSets (ValueSet a) (ValueSet b) =
    a
        == b
        || (a.ints
                == b.ints
                && a.floats
                == b.floats
                && a.strings
                == b.strings
                && a.nil
                == b.nil
                && a.true
                == b.true
                && a.false
                == b.false
                && a.keywords
                == b.keywords
                && a.symbols
                == b.symbols
                && areEqualLists a.otherValues b.otherValues
           )


areEqualValues : Value io -> Value io -> Bool
areEqualValues a b =
    if a == b then
        -- referential equality
        True

    else
        -- different metadata
        case ( a, b ) of
            ( List listA, List listB ) ->
                areEqualLocatedLists listA listB

            ( MapEntry ( keyA, Located _ valA ), Vector vB ) ->
                case Array.toList vB of
                    [ Located _ keyB, Located _ valB ] ->
                        areEqualValues keyA keyB && areEqualValues valA valB

                    _ ->
                        False

            ( Vector _, MapEntry _ ) ->
                areEqualValues b a

            ( MapEntry ( keyA, Located _ valA ), MapEntry ( keyB, Located _ valB ) ) ->
                areEqualValues keyA keyB && areEqualValues valA valB

            ( Vector vectorA, Vector vectorB ) ->
                areEqualLocatedLists (Array.toList vectorA) (Array.toList vectorB)

            ( Map mapA, Map mapB ) ->
                areEqualMaps mapA mapB

            ( Set setA, Set setB ) ->
                areEqualSets setA setB

            _ ->
                False


linearFind : (a -> Bool) -> List a -> Maybe a
linearFind f l =
    case l of
        [] ->
            Nothing

        a :: rest ->
            if f a then
                Just a

            else
                linearFind f rest
