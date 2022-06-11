module Enclojure.Lib exposing (init, prelude)

import Array
import Enclojure.Callable as Callable exposing (toArityFunction)
import Enclojure.Common as Common
    exposing
        ( Arity(..)
        , Callable
        , Continuation
        , Env
        , Exception(..)
        , IO(..)
        , Number(..)
        , Ref(..)
        , Thunk(..)
        , Value(..)
        )
import Enclojure.Json
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime
import Enclojure.Value as Value exposing (inspect, inspectType)
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Regex


emptyCallable : Callable io
emptyCallable =
    Callable.new


init : Env io -> Env io
init env =
    [ ( "+", plus )
    , ( "-", minus )
    , ( "/", div )
    , ( "*", mul )
    , ( "=", isEqual )
    , ( "not=", isNotEqual )
    , ( ">", isGreaterThan )
    , ( ">=", isGreaterThanOrEqual )
    , ( "<", isLessThan )
    , ( "<=", isLessThanOrEqual )
    , ( "apply", apply )
    , ( "assoc", assoc )
    , ( "atom", atom )
    , ( "conj", conj )
    , ( "cons", cons )
    , ( "contains?", contains )
    , ( "count", count )
    , ( "deref", deref )
    , ( "dissoc", dissoc )
    , ( "Exception.", newException )
    , ( "empty", empty )
    , ( "first", first )
    , ( "float", toFloat_ )
    , ( "float?", isFloat )
    , ( "get", get )
    , ( "json/encode", jsonEncode )
    , ( "json/decode", jsonDecode )
    , ( "int", toInt )
    , ( "int?", isInteger )
    , ( "integer?", isInteger )
    , ( "key", key_ )
    , ( "keys", keys )
    , ( "keyword?", isKeyword )
    , ( "list", list )
    , ( "list?", isList )
    , ( "map?", isMap )
    , ( "map-entry?", isMapEntry )
    , ( "not", not_ )
    , ( "number?", isNumber )
    , ( "nth", nth )
    , ( "peek", peek )
    , ( "pop", pop )
    , ( "pr-str", prStr )
    , ( "rem", rem )
    , ( "re-find", reFind )
    , ( "re-matches", reMatches )
    , ( "re-seq", reSeq )
    , ( "reset!", reset )
    , ( "rest", rest_ )
    , ( "second", second )
    , ( "seq", seq )
    , ( "set", set )
    , ( "set?", isSet )
    , ( "str", str )
    , ( "symbol?", isSymbol )
    , ( "swap!", swap )
    , ( "throw", throw )
    , ( "val", val_ )
    , ( "vals", vals )
    , ( "vec", vec )
    , ( "vector", vector )
    , ( "vector?", isVector )
    ]
        |> List.foldl
            (\( name, fn ) aEnv ->
                Runtime.bindGlobal name (Fn { name = Just name, doc = fn.doc, signatures = Callable.signatures fn } (Common.toThunk fn)) aEnv
            )
            env


atom : Callable io
atom =
    let
        arity1 v env k =
            let
                ( newEnv, atomId ) =
                    Runtime.addAtom v env
            in
            ( Ok ( Const (Ref (Atom atomId)), newEnv ), Just (Thunk k) )
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") arity1
        , doc = Just "Creates and returns an Atom with an initial value of x."
    }


deref : Callable io
deref =
    let
        arity1 v env k =
            Value.tryRef v
                |> Maybe.map
                    (\ref ->
                        ( Ok ( Const (Runtime.deref ref env), env ), Just (Thunk k) )
                    )
                |> Maybe.withDefault
                    ( Err ( Value.exception "type error: deref expects a ref as its argument", env )
                    , Just (Thunk k)
                    )
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "ref") arity1
        , doc = Just "Also reader macro: @var/@atom. When applied to a var or atom, returns its current state."
    }


reset : Callable io
reset =
    let
        arity2 ( refVal, valVal ) env k =
            case Value.tryAtom refVal of
                Just atomId ->
                    ( Ok ( Const valVal, env |> Runtime.resetAtom atomId valVal )
                    , Just (Thunk k)
                    )

                Nothing ->
                    ( Err ( Value.exception "type error: reset! expects an atom as its first argument", env )
                    , Just (Thunk k)
                    )
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "atom", Symbol "newval" ) arity2
        , doc = Just "Sets the value of atom to newval without regard for the current value. Returns newval."
    }


swap : Callable io
swap =
    let
        arity2 arity env k =
            let
                ( refVal, fnVal ) =
                    arity.args

                fnArgs =
                    arity.rest
            in
            case Value.tryAtom refVal of
                Just atomId ->
                    let
                        atomValue =
                            Runtime.deref (Atom atomId) env
                    in
                    Runtime.apply
                        (Located.unknown fnVal)
                        (Located.unknown (Value.list (atomValue :: fnArgs)))
                        env
                        (\(Located loc retVal) retEnv ->
                            Located loc
                                ( Ok ( Const retVal, retEnv |> Runtime.resetAtom atomId retVal )
                                , Just (Thunk k)
                                )
                        )
                        |> Located.getValue

                Nothing ->
                    ( Err ( Value.exception "type error: swap! expects an atom as its first argument", env )
                    , Just (Thunk k)
                    )
    in
    { emptyCallable
        | arity2 = Just <| Variadic { argNames = ( Symbol "atom", Symbol "f" ), restArgName = Symbol "args" } arity2
        , doc = Just """Atomically swaps the value of atom to be: (apply f current-value-of-atom args).
Returns the value that was swapped in."""
    }


jsonEncode : Callable io
jsonEncode =
    let
        arity1 v =
            Enclojure.Json.encodeToString v |> String
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| toArityFunction (arity1 >> Const >> Ok)
        , doc = Just "Encode x as a JSON string."
    }


jsonDecode : Callable io
jsonDecode =
    let
        arity1 v =
            Value.tryString v
                |> Result.fromMaybe (Exception "type error: json/decode expects a string" [])
                |> Result.andThen Enclojure.Json.decodeFromString
                |> Result.map Const
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "s") <| toArityFunction arity1
        , doc = Just "Attempt to decode a JSON string s as an Enclojure value."
    }


not_ : Callable io
not_ =
    let
        arity1 val =
            Ok (Const (Bool (not (Value.isTruthy val))))
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "x") (toArityFunction arity1))
        , doc = Just "Returns true if x is logical false, false otherwise."
    }


list : Callable io
list =
    let
        arity0 { rest } =
            Ok (Const (List (List.map Located.unknown rest)))
    in
    { emptyCallable
        | arity0 = Just (Variadic { argNames = (), restArgName = Symbol "items" } (toArityFunction arity0))
        , doc = Just "Creates a new list containing the items."
    }


toNumbers : List (Value io) -> Result Exception (List Number)
toNumbers =
    List.foldl
        (\e a -> a |> Result.andThen (\l -> toNumber e |> Result.map (\n -> n :: l)))
        (Ok [])
        >> Result.map List.reverse


flip : (a -> b -> c) -> (b -> a -> c)
flip f =
    \a b -> f b a


varargOp : { arity0 : Maybe Number, arity1 : Maybe (Number -> Number), arity2 : Number -> Number -> Number, doc : Maybe String } -> Callable io
varargOp { arity0, arity1, arity2, doc } =
    let
        wrappedArity2 { args, rest } =
            let
                ( argA, argB ) =
                    args
            in
            Result.map3
                (\a b ->
                    List.foldr (flip arity2) (arity2 a b) >> Number
                )
                (toNumber argA)
                (toNumber argB)
                (toNumbers rest)
    in
    { emptyCallable
        | arity0 = arity0 |> Maybe.map (Number >> Const >> Ok >> always >> toArityFunction >> Fixed ())
        , arity1 =
            arity1
                |> Maybe.map
                    (\fn ->
                        Fixed (Symbol "x") <| toArityFunction <| (toNumber >> Result.map (fn >> Number >> Const))
                    )
        , arity2 =
            (wrappedArity2 >> Result.map Const)
                |> toArityFunction
                |> Variadic
                    { argNames = ( Symbol "x", Symbol "y" )
                    , restArgName = Symbol "more"
                    }
                |> Just
        , doc = doc
    }


plus : Callable io
plus =
    varargOp
        { arity0 = Just (Int 0)
        , arity1 = Just identity
        , arity2 =
            addNumbers
        , doc = Just "Returns the sum of nums. (+) returns 0."
        }


addNumbers : Number -> Number -> Number
addNumbers =
    \numA numB ->
        case ( numA, numB ) of
            ( Int a, Int b ) ->
                Int (a + b)

            ( Int a, Float b ) ->
                Float (toFloat a + b)

            ( Float a, Int b ) ->
                Float (a + toFloat b)

            ( Float a, Float b ) ->
                Float (a + b)


negateNumber : Number -> Number
negateNumber numX =
    case numX of
        Int x ->
            Int (negate x)

        Float x ->
            Float (negate x)


minus : Callable io
minus =
    varargOp
        { arity0 = Nothing
        , arity1 = Just negateNumber
        , arity2 =
            \numA numB ->
                case ( numA, numB ) of
                    ( Int a, Int b ) ->
                        Int (a - b)

                    ( Int a, Float b ) ->
                        Float (toFloat a - b)

                    ( Float a, Int b ) ->
                        Float (a - toFloat b)

                    ( Float a, Float b ) ->
                        Float (a - b)
        , doc = Just "If no ys are supplied, returns the negation of x, else subtracts the ys from x and returns the result."
        }


mul : Callable io
mul =
    varargOp
        { arity0 = Just (Int 1)
        , arity1 = Just identity
        , arity2 =
            \numA numB ->
                case ( numA, numB ) of
                    ( Int a, Int b ) ->
                        Int (a * b)

                    ( Int a, Float b ) ->
                        Float (toFloat a * b)

                    ( Float a, Int b ) ->
                        Float (a * toFloat b)

                    ( Float a, Float b ) ->
                        Float (a * b)
        , doc = Just "Returns the product of nums. (*) returns 1."
        }


div : Callable io
div =
    let
        op numA numB =
            case ( numA, numB ) of
                ( Int a, Int b ) ->
                    Int (a // b)

                ( Int a, Float b ) ->
                    Float (toFloat a / b)

                ( Float a, Int b ) ->
                    Float (a / toFloat b)

                ( Float a, Float b ) ->
                    Float (a / b)
    in
    varargOp
        { arity0 = Nothing
        , arity1 = Just (op (Int 1))
        , arity2 = op
        , doc = Just "If no denominators are supplied, returns 1/numerator, else returns numerator divided by all of the denominators."
        }


remainderByFloat : Float -> Float -> Float
remainderByFloat by x =
    x - (toFloat (floor (x / by)) * by)


rem : Callable io
rem =
    let
        op numA numB =
            case ( numA, numB ) of
                ( Int a, Int b ) ->
                    Int (remainderBy b a)

                ( Int a, Float b ) ->
                    Float (remainderByFloat b (toFloat a))

                ( Float a, Int b ) ->
                    Float (remainderByFloat (toFloat b) a)

                ( Float a, Float b ) ->
                    Float (remainderByFloat b a)

        arity2 ( valA, valB ) =
            Result.map2 op
                (toNumber valA)
                (toNumber valB)
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "x", Symbol "b" ) <| toArityFunction <| (arity2 >> Result.map (Number >> Const))
        , doc = Just "Returns remainder of dividing numerator by denominator."
    }


toNumber : Value io -> Result Exception Number
toNumber val =
    case val of
        Number n ->
            Ok n

        _ ->
            Err <| Exception (inspect val ++ " is not a number") []


isLessThan : Callable io
isLessThan =
    compOp
        { intOp = (<)
        , floatOp = (<)
        , stringOp = (<)
        , doc = Just "Returns non-nil if nums or strings are in monotonically increasing order, otherwise false."
        }


isLessThanOrEqual : Callable io
isLessThanOrEqual =
    compOp
        { intOp = (<=)
        , floatOp = (<=)
        , stringOp = (<=)
        , doc = Just "Returns non-nil if nums or strings are in monotonically non-decreasing order, otherwise false."
        }


isGreaterThan : Callable io
isGreaterThan =
    compOp
        { intOp = (>)
        , floatOp = (>)
        , stringOp = (>)
        , doc = Just "Returns non-nil if nums are in monotonically decreasing order, otherwise false."
        }


isGreaterThanOrEqual : Callable io
isGreaterThanOrEqual =
    compOp
        { intOp = (>=)
        , floatOp = (>=)
        , stringOp = (>=)
        , doc = Just "Returns non-nil if nums are in monotonically non-increasing order, otherwise false."
        }


compOp :
    { intOp : Int -> Int -> Bool
    , floatOp : Float -> Float -> Bool
    , stringOp : String -> String -> Bool
    , doc : Maybe String
    }
    -> Callable io
compOp { intOp, floatOp, stringOp, doc } =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                result =
                    case args of
                        ( Number (Float a), Number (Float b) ) ->
                            Ok (floatOp a b)

                        ( Number (Float a), Number (Int b) ) ->
                            Ok (floatOp a (toFloat b))

                        ( Number (Int a), Number (Float b) ) ->
                            Ok (floatOp (toFloat a) b)

                        ( Number (Int a), Number (Int b) ) ->
                            Ok (intOp a b)

                        ( String a, String b ) ->
                            Ok (stringOp a b)

                        ( a, b ) ->
                            Err (Exception ("can't compare " ++ inspect a ++ " and " ++ inspect b) [])
            in
            case rest of
                [] ->
                    result |> Result.map Bool

                nextVal1 :: nextRest ->
                    result
                        |> Result.andThen
                            (\r ->
                                if r then
                                    arity2 { args = ( Tuple.second args, nextVal1 ), rest = nextRest }

                                else
                                    Ok (Bool False)
                            )
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "x") (toArityFunction (arity1 >> Result.map Const)))
        , arity2 =
            Just
                (Variadic { argNames = ( Symbol "x", Symbol "y" ), restArgName = Symbol "more" }
                    (toArityFunction (arity2 >> Result.map Const))
                )
        , doc = doc
    }


isEqual : Callable io
isEqual =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (Value.isEqual a b))

                nextVal1 :: nextRest ->
                    if Value.isEqual a b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "x") (toArityFunction (arity1 >> Result.map Const)))
        , arity2 =
            Just
                (Variadic { argNames = ( Symbol "x", Symbol "y" ), restArgName = Symbol "more" }
                    (toArityFunction (arity2 >> Result.map Const))
                )
    }


isNotEqual : Callable io
isNotEqual =
    let
        arity1 _ =
            Ok (Bool False)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (a /= b))

                nextVal1 :: nextRest ->
                    if a /= b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "x") (toArityFunction (arity1 >> Result.map Const)))
        , arity2 =
            Just
                (Variadic { argNames = ( Symbol "x", Symbol "y" ), restArgName = Symbol "more" }
                    (toArityFunction (arity2 >> Result.map Const))
                )
        , doc = Just "Same as (not (= x y))"
    }


isSymbol : Callable io
isSymbol =
    let
        arity1 v =
            v
                |> Value.trySymbol
                |> Maybe.map (always True)
                |> Maybe.withDefault False
                |> Bool
                |> Const
                |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a Symbol"
    }


isKeyword : Callable io
isKeyword =
    let
        arity1 v =
            v
                |> Value.tryKeyword
                |> Maybe.map (always True)
                |> Maybe.withDefault False
                |> Bool
                |> Const
                |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a Keyword"
    }


isList : Callable io
isList =
    let
        arity1 v =
            case v of
                List _ ->
                    Bool True |> Const |> Ok

                _ ->
                    Bool False |> Const |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a List"
    }


isVector : Callable io
isVector =
    let
        arity1 v =
            case v of
                Vector _ ->
                    Bool True |> Const |> Ok

                _ ->
                    Bool False |> Const |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a Vector"
    }


set : Callable io
set =
    let
        arity1 val =
            case val of
                Set _ ->
                    Ok (Const val)

                _ ->
                    Value.toSeq val
                        |> Result.map (List.map Located.getValue >> ValueSet.fromList >> Set >> Const)
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction arity1
        , doc = Just "Returns a set of the distinct elements of coll."
    }


isSet : Callable io
isSet =
    let
        arity1 v =
            case v of
                Set _ ->
                    Bool True |> Const |> Ok

                _ ->
                    Bool False |> Const |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a Set"
    }


isMap : Callable io
isMap =
    let
        arity1 v =
            case v of
                Map _ ->
                    Bool True |> Const |> Ok

                _ ->
                    Bool False |> Const |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a Map"
    }


isMapEntry : Callable io
isMapEntry =
    let
        arity1 v =
            case v of
                MapEntry _ ->
                    Bool True |> Const |> Ok

                _ ->
                    Bool False |> Const |> Ok
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
        , doc = Just "Return true if x is a MapEntry"
    }


str : Callable io
str =
    let
        arity0 { rest } =
            rest
                |> List.map Value.toString
                |> String.join ""
                |> String
                |> Ok
    in
    { emptyCallable
        | arity0 =
            Just
                (Variadic { argNames = (), restArgName = Symbol "xs" }
                    (toArityFunction (arity0 >> Result.map Const))
                )
        , doc = Just """With no args, returns the empty string. With one arg x, returns a string representation of x.
(str nil) returns the empty string.
With more than one arg, returns the concatenation of the str values of the args."""
    }


prStr : Callable io
prStr =
    let
        arity0 { rest } =
            rest
                |> List.map Value.inspect
                |> String.join ""
                |> String
                |> Ok
    in
    { emptyCallable
        | arity0 =
            Just
                (Variadic { argNames = (), restArgName = Symbol "xs" }
                    (toArityFunction (arity0 >> Result.map Const))
                )
        , doc = Just "Prints the object(s) to a string. Prints the object(s), separated by spaces if there is more than one. Prints in a way that objects can be read by the reader"
    }


seq : Callable io
seq =
    let
        arity1 coll =
            case coll of
                List [] ->
                    Ok Nil

                List l ->
                    Ok (List l)

                Vector l ->
                    if Array.isEmpty l then
                        Ok Nil

                    else
                        Ok (List (Array.toList l))

                Set s ->
                    if ValueSet.isEmpty s then
                        Ok Nil

                    else
                        Ok (List (ValueSet.toList s |> List.map Located.unknown))

                String s ->
                    if String.length s == 0 then
                        Ok Nil

                    else
                        Ok (List (String.toList s |> List.map (String.fromChar >> String >> Located.unknown)))

                Map m ->
                    if ValueMap.isEmpty m then
                        Ok Nil

                    else
                        Ok (List (ValueMap.toList m |> List.map (MapEntry >> Located.unknown)))

                Nil ->
                    Ok Nil

                _ ->
                    Err (Exception (inspect coll ++ " is not sequable") [])
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") (toArityFunction (arity1 >> Result.map Const)))
        , doc = Just """Returns a seq (list) on the collection. If the collection is empty, returns nil.
(seq nil) returns nil.
seq also works on strings."""
    }


fixedCall : Maybe (Arity io a) -> a -> Env io -> Continuation io -> ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) )
fixedCall mArity =
    mArity
        |> Maybe.andThen
            (\arity ->
                case arity of
                    Fixed _ a ->
                        Just a

                    Variadic _ _ ->
                        Nothing
            )
        |> Maybe.withDefault
            (\_ env k ->
                ( Err
                    ( Value.exception "Interpreter error: undefined internal call arity" |> Runtime.throw env
                    , env
                    )
                , Just (Thunk k)
                )
            )


cons : Callable io
cons =
    let
        arity2 ( x, coll ) env1 k =
            fixedCall
                seq.arity1
                coll
                env1
                (\(Located collLoc collSeq) env2 ->
                    case collSeq of
                        List l ->
                            Located collLoc ( Ok ( Const (List (Located.unknown x :: l)), env2 ), Just (Thunk k) )

                        Nil ->
                            Located.unknown
                                ( Ok ( Const (List [ Located.unknown x ]), env2 )
                                , Just (Thunk k)
                                )

                        _ ->
                            Located.unknown
                                ( Err
                                    ( Exception "Interpreter error: seq returned a non-list" []
                                    , env2
                                    )
                                , Just (Thunk k)
                                )
                )
    in
    { emptyCallable
        | arity2 = Just (Fixed ( Symbol "x", Symbol "seq" ) arity2)
        , doc = Just "Returns a new seq where x is the first element and seq is the rest."
    }


conj : Callable io
conj =
    let
        arity2 signature =
            let
                ( coll, x ) =
                    signature.args

                xs =
                    x :: signature.rest
            in
            case coll of
                List l ->
                    xs
                        |> List.map Located.unknown
                        |> List.foldl (::) l
                        |> List
                        |> Ok

                Vector a ->
                    xs
                        |> List.map Located.unknown
                        |> List.foldl Array.push a
                        |> Vector
                        |> Ok

                Nil ->
                    Ok <| List (xs |> List.map Located.unknown |> List.reverse)

                Set s ->
                    xs
                        |> List.foldl ValueSet.insert s
                        |> Set
                        |> Ok

                Map m ->
                    xs
                        |> List.foldl
                            (\e a ->
                                case e of
                                    MapEntry ( k, v ) ->
                                        a |> Result.map (ValueMap.insert k v)

                                    Vector array ->
                                        case Array.toList array of
                                            [ k, v ] ->
                                                a |> Result.map (ValueMap.insert (Located.getValue k) v)

                                            _ ->
                                                Err (Exception "Vector arg to map conj must be a pair" [])

                                    _ ->
                                        Err <| Exception (inspect e ++ " is not a valid map entry") []
                            )
                            (Ok m)
                        |> Result.map Map

                _ ->
                    Err (Exception ("don't know how to conj to " ++ inspect coll) [])
    in
    { emptyCallable
        | arity2 =
            Just <|
                Variadic { argNames = ( Symbol "coll", Symbol "x" ), restArgName = Symbol "xs" } <|
                    toArityFunction (arity2 >> Result.map Const)
        , doc = Just """conj[oin]. Returns a new collection with the xs
'added'. (conj nil item) returns (item).
(conj coll) returns coll. (conj) returns [].
The 'addition' may happen at different 'places' depending
on the concrete type."""
    }


contains : Callable io
contains =
    let
        arity2 ( coll, x ) =
            case coll of
                List _ ->
                    Err (Exception "contains? not supported on lists" [])

                Vector a ->
                    case x of
                        Number (Int i) ->
                            Ok (Bool (i >= 0 && i < Array.length a))

                        _ ->
                            Ok (Bool False)

                Nil ->
                    Ok (Bool False)

                Set s ->
                    ValueSet.member x s |> Bool |> Ok

                Map m ->
                    ValueMap.member x m |> Bool |> Ok

                _ ->
                    Err (Exception ("don't know how to conj to " ++ inspect coll) [])
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "coll", Symbol "key" ) <| toArityFunction (arity2 >> Result.map Const)
        , doc = Just """Returns true if key is present in the given collection, otherwise
returns false.  Note that for numerically indexed collections like
vectors, this tests if the numeric key is within the
range of indexes. 'contains?' operates constant or logarithmic time;
it will not perform a linear search for a value. See also 'some'."""
    }


first : Callable io
first =
    let
        arity1 collVal =
            collVal
                |> Value.toSeq
                |> Result.map (List.map Located.getValue)
                |> Result.map
                    (\s ->
                        case s of
                            x :: _ ->
                                x

                            [] ->
                                Nil
                    )
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") <| toArityFunction (arity1 >> Result.map Const))
    }


second : Callable io
second =
    let
        arity1 collVal =
            collVal
                |> Value.toSeq
                |> Result.map (List.map Located.getValue)
                |> Result.map
                    (\s ->
                        case s of
                            _ :: x :: _ ->
                                x

                            _ ->
                                Nil
                    )
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") <| toArityFunction (arity1 >> Result.map Const))
    }


peek : Callable io
peek =
    let
        arity1 val =
            case val of
                List l ->
                    Ok <| (List.head l |> Maybe.map Located.getValue |> Maybe.withDefault Nil)

                Vector v ->
                    Ok <| (Array.get (Array.length v - 1) v |> Maybe.map Located.getValue |> Maybe.withDefault Nil)

                Nil ->
                    Ok Nil

                _ ->
                    Err <| Exception ("Cannot use " ++ inspectType val ++ " as a queue") []
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") (toArityFunction (arity1 >> Result.map Const)))
    }


pop : Callable io
pop =
    let
        arity1 val =
            case val of
                List l ->
                    List.tail l
                        |> Maybe.map List
                        |> Result.fromMaybe (Value.exception "Can't pop an empty list")

                Vector v ->
                    if Array.isEmpty v then
                        Err (Value.exception "Can't pop an empty vector")

                    else
                        Array.slice 0 -1 v |> Vector |> Ok

                Nil ->
                    Ok Nil

                _ ->
                    Err <| Exception ("Can't use " ++ inspectType val ++ " as a queue") []
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") (toArityFunction (arity1 >> Result.map Const)))
    }


isNumber : Callable io
isNumber =
    let
        arity1 v =
            case v of
                Number _ ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| toArityFunction (arity1 >> Const >> Ok)
    }


isInteger : Callable io
isInteger =
    let
        arity1 v =
            case v of
                Number (Int _) ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| toArityFunction (arity1 >> Const >> Ok)
    }


isFloat : Callable io
isFloat =
    let
        arity1 v =
            case v of
                Number (Float _) ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| toArityFunction (arity1 >> Const >> Ok)
    }


rest_ : Callable io
rest_ =
    let
        arity1 coll env1 k =
            fixedCall
                seq.arity1
                coll
                env1
                (\(Located collLoc collSeq) env2 ->
                    case collSeq of
                        List (_ :: rst) ->
                            Located collLoc ( Ok ( Const (List rst), env2 ), Just (Thunk k) )

                        List _ ->
                            Located collLoc ( Ok ( Const Nil, env2 ), Just (Thunk k) )

                        Nil ->
                            Located.unknown
                                ( Ok ( Const (List []), env2 )
                                , Just (Thunk k)
                                )

                        _ ->
                            Located.unknown
                                ( Err
                                    ( Value.exception "Interpreter error: seq returned a non-list"
                                        |> Runtime.throw env2
                                    , env2
                                    )
                                , Just (Thunk k)
                                )
                )
    in
    { emptyCallable
        | arity1 = Just (Fixed (Symbol "coll") arity1)
    }


throw : Callable io
throw =
    let
        arity1 v =
            case v of
                Throwable e ->
                    Err e

                _ ->
                    Err (Exception (inspect v ++ " is not throwable") [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "ex") <| toArityFunction arity1
    }


newException : Callable io
newException =
    let
        arity1 v =
            case v of
                String s ->
                    Ok (Throwable (Exception s []))

                _ ->
                    Err (Exception "exception message must be a string" [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "msg") <| toArityFunction (arity1 >> Result.map Const)
    }



-- ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )


apply : Callable io
apply =
    let
        arity2 signature env k =
            let
                ( fn, firstArg ) =
                    signature.args

                args =
                    firstArg :: signature.rest

                numArgs =
                    List.length args

                posArgs =
                    List.take (numArgs - 1) args

                listArgsResult =
                    args
                        |> List.drop (numArgs - 1)
                        |> List.head
                        |> Result.fromMaybe (Exception "Interpreter error: arity2 function doesn't have a 2nd argument" [])
                        |> Result.andThen Value.toSeq
            in
            case listArgsResult of
                Ok listArgs ->
                    Runtime.apply
                        (Located.unknown fn)
                        (Located.unknown (List (List.map Located.unknown posArgs ++ listArgs)))
                        env
                        k
                        |> Located.getValue

                Err e ->
                    ( Err ( e, env ), Just (Thunk k) )
    in
    { emptyCallable
        | arity2 = Just <| Variadic { argNames = ( Symbol "f", Symbol "x" ), restArgName = Symbol "args" } arity2
    }


get : Callable io
get =
    let
        arity2 ( mapVal, key ) =
            arity3 ( mapVal, key, Nil )

        arity3 ( mapVal, key, default ) =
            (case mapVal of
                Map m ->
                    ValueMap.get key m |> Maybe.map Located.getValue

                Vector l ->
                    case key of
                        Number (Int i) ->
                            l |> Array.get i |> Maybe.map Located.getValue

                        _ ->
                            Nothing

                Set s ->
                    if ValueSet.member key s then
                        Just key

                    else
                        Nothing

                String s ->
                    case key of
                        Number (Int i) ->
                            if i < String.length s then
                                s |> String.slice i (i + 1) |> String |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                Nil ->
                    Nothing

                _ ->
                    Just Nil
            )
                |> Maybe.withDefault default
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "map", Symbol "key" ) <| toArityFunction (arity2 >> Const >> Ok)
        , arity3 =
            Just <|
                Fixed ( Symbol "map", Symbol "key", Symbol "not-found" ) <|
                    toArityFunction (arity3 >> Const >> Ok)
    }


listToPairs : List a -> Maybe (List ( a, a ))
listToPairs l =
    case l of
        [] ->
            Just []

        a :: b :: rest ->
            listToPairs rest |> Maybe.map ((::) ( a, b ))

        _ ->
            Nothing


assoc : Callable io
assoc =
    let
        arity3 signature =
            let
                ( val, firstK, firstV ) =
                    signature.args

                kvs =
                    listToPairs signature.rest
                        |> Maybe.map ((::) ( firstK, firstV ))
                        |> Result.fromMaybe (Exception "invalid number of key/value args to assoc" [])

                keysToInt l =
                    case l of
                        [] ->
                            Ok []

                        ( k, v ) :: rest ->
                            case k of
                                Number (Int i) ->
                                    keysToInt rest |> Result.map ((::) ( i, v ))

                                _ ->
                                    Err (Exception "Key must be integer" [])
            in
            case val of
                Map m ->
                    kvs
                        |> Result.map (List.foldr (\( k, v ) a -> ValueMap.insert k (Located.unknown v) a) m)
                        |> Result.map Map

                Vector array ->
                    kvs
                        |> Result.andThen keysToInt
                        |> Result.andThen
                            (List.foldl
                                (\( k, v ) a ->
                                    a
                                        |> Result.andThen
                                            (\arr ->
                                                if k == Array.length arr then
                                                    Ok <| Array.push (Located.unknown v) arr

                                                else if k < Array.length arr then
                                                    Ok <| Array.set k (Located.unknown v) arr

                                                else
                                                    Err <| Exception "index out of bounds" []
                                            )
                                )
                                (Ok array)
                            )
                        |> Result.map Vector

                Nil ->
                    kvs
                        |> Result.map (List.foldr (\( k, v ) a -> ValueMap.insert k (Located.unknown v) a) ValueMap.empty)
                        |> Result.map Map

                _ ->
                    Err (Exception (inspect val ++ " is not associable") [])
    in
    { emptyCallable
        | arity3 =
            Just <|
                Variadic
                    { argNames = ( Symbol "map", Symbol "key", Symbol "val" )
                    , restArgName = Symbol "kvs"
                    }
                <|
                    toArityFunction (arity3 >> Result.map Const)
    }


dissoc : Callable io
dissoc =
    let
        arity2 signature =
            let
                ( val, firstKey ) =
                    signature.args

                dissocKeys =
                    firstKey :: signature.rest
            in
            case val of
                Map m ->
                    dissocKeys
                        |> List.foldr (\k a -> ValueMap.remove k a) m
                        |> Map
                        |> Ok

                Nil ->
                    Ok Nil

                _ ->
                    Err (Exception (inspect val ++ " is not dissociable") [])
    in
    { emptyCallable | arity2 = Just <| Variadic { argNames = ( Symbol "map", Symbol "key" ), restArgName = Symbol "ks" } <| toArityFunction (arity2 >> Result.map Const) }


key_ : Callable io
key_ =
    let
        arity1 v =
            case v of
                MapEntry ( k, _ ) ->
                    Ok (Const k)

                _ ->
                    Err (Exception (inspect v ++ " is not a map entry") [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "e") <| toArityFunction arity1
    }


val_ : Callable io
val_ =
    let
        arity1 v =
            case v of
                MapEntry ( _, Located _ value ) ->
                    Ok (Const value)

                _ ->
                    Err (Exception (inspect v ++ " is not a map entry") [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "e") <| toArityFunction arity1
    }


reSeq : Callable io
reSeq =
    let
        arity2 ( reValue, sValue ) =
            Result.map2
                (\re s -> Regex.find re s |> List.map (.match >> String) |> Value.list |> Const)
                (Value.tryRegex reValue |> Result.fromMaybe (Value.exception "first argument to re-seq must be a regular expression"))
                (Value.tryString sValue |> Result.fromMaybe (Value.exception "second argument to re-seq must be a string"))
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "re", Symbol "s" ) <| toArityFunction arity2
    }


reFind : Callable io
reFind =
    let
        arity2 ( reValue, sValue ) =
            Result.map2
                (\re s -> Regex.findAtMost 1 re s |> List.head |> Maybe.map (.match >> String) |> Maybe.withDefault Nil |> Const)
                (Value.tryRegex reValue |> Result.fromMaybe (Value.exception "first argument to re-seq must be a regular expression"))
                (Value.tryString sValue |> Result.fromMaybe (Value.exception "second argument to re-seq must be a string"))
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "re", Symbol "s" ) <| toArityFunction arity2
    }


reMatches : Callable io
reMatches =
    let
        arity2 ( reValue, sValue ) =
            Result.map2
                (\re s ->
                    Regex.findAtMost 1 re s
                        |> List.head
                        |> Maybe.map
                            (\match ->
                                if match.match /= s then
                                    Ok (Const Nil)

                                else if List.isEmpty match.submatches then
                                    Ok (Const (String match.match))

                                else
                                    (String match.match :: List.filterMap (Maybe.map String) match.submatches)
                                        |> Value.list
                                        |> Const
                                        |> Ok
                            )
                        |> Maybe.withDefault (Ok <| Const <| Nil)
                )
                (Value.tryRegex reValue |> Result.fromMaybe (Value.exception "first argument to re-matches must be a regular expression"))
                (Value.tryString sValue |> Result.fromMaybe (Value.exception "second argument to re-matches must be a string"))
                |> Result.andThen identity
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "re", Symbol "s" ) <| Callable.toArityFunction arity2
    }


toInt : Callable io
toInt =
    let
        arity1 xVal =
            case xVal of
                Number (Float x) ->
                    x |> floor |> Int |> Number |> Const |> Ok

                Number (Int _) ->
                    xVal |> Const |> Ok

                _ ->
                    Err (Value.exception "type error: int expects a numeric argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
    }


toFloat_ : Callable io
toFloat_ =
    let
        arity1 xVal =
            case xVal of
                Number (Float _) ->
                    xVal |> Const |> Ok

                Number (Int x) ->
                    x |> toFloat |> Float |> Number |> Const |> Ok

                _ ->
                    Err (Value.exception "type error: float expects a numeric argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction arity1
    }


count : Callable io
count =
    let
        arity1 collVal =
            case collVal of
                Vector array ->
                    Array.length array |> Int |> Number |> Const |> Ok

                List l ->
                    List.length l |> Int |> Number |> Const |> Ok

                String string ->
                    String.length string |> Int |> Number |> Const |> Ok

                Map valueMap ->
                    valueMap |> ValueMap.toList |> List.length |> Int |> Number |> Const |> Ok

                Set valueSet ->
                    valueSet |> ValueSet.toList |> List.length |> Int |> Number |> Const |> Ok

                Nil ->
                    Int 0 |> Number |> Const |> Ok

                _ ->
                    Err (Value.exception "type error: count expects a collection or a string")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction arity1
        , doc = Just "Returns the number of items in coll. (count nil) returns 0. Also works on strings."
    }


empty : Callable io
empty =
    let
        arity1 collVal =
            case collVal of
                List _ ->
                    List [] |> Const |> Ok

                Vector _ ->
                    Array.empty |> Vector |> Const |> Ok

                Map _ ->
                    ValueMap.empty |> Map |> Const |> Ok

                Set _ ->
                    ValueSet.empty |> Set |> Const |> Ok

                String _ ->
                    String "" |> Const |> Ok

                Nil ->
                    Const Nil |> Ok

                _ ->
                    Err (Value.exception "type error: empty expects a collection argument")
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction arity1
        , doc = Just "Returns an empty collection of the same type as coll, or nil."
    }


keys : Callable io
keys =
    let
        arity1 mVal =
            mVal
                |> Value.tryMap
                |> Result.fromMaybe (Value.exception "keys expects a single map argument")
                |> Result.map (ValueMap.keys >> Value.list >> Const)
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "map") <| Callable.toArityFunction arity1
        , doc = Just "Returns a list of the map's keys, in the same order as (seq map)."
    }


vals : Callable io
vals =
    let
        arity1 mVal =
            mVal
                |> Value.tryMap
                |> Result.fromMaybe (Value.exception "vals expects a single map argument")
                |> Result.map (ValueMap.values >> Value.list >> Const)
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "map") <| Callable.toArityFunction arity1
        , doc = Just "Returns a list of the map's values, in the same order as (seq map)."
    }


nth_ : Value io -> Value io -> Maybe (Value io) -> Result Exception (IO io)
nth_ collVal indexVal defaultVal =
    indexVal
        |> Value.tryInt
        |> Result.fromMaybe (Value.exception "type error: nth expects an integer index")
        |> Result.andThen
            (\index ->
                case collVal of
                    String s ->
                        if String.length s <= index then
                            defaultVal
                                |> Maybe.map Const
                                |> Result.fromMaybe (Value.exception "index out of bounds")

                        else
                            String.slice index (index + 1) s |> String |> Const |> Ok

                    Vector a ->
                        case Array.get index a of
                            Just v ->
                                v |> Located.getValue |> Const |> Ok

                            Nothing ->
                                defaultVal
                                    |> Maybe.map Const
                                    |> Result.fromMaybe (Value.exception "index out of bounds")

                    _ ->
                        Err (Value.exception "type error: nth is not supported on this type")
            )


nth : Callable io
nth =
    let
        arity2 ( collVal, indexVal ) =
            nth_ collVal indexVal Nothing

        arity3 ( collVal, indexVal, defaultVal ) =
            nth_ collVal indexVal (Just defaultVal)
    in
    { emptyCallable
        | arity2 = Just <| Fixed ( Symbol "coll", Symbol "index" ) <| Callable.toArityFunction arity2
        , arity3 = Just <| Fixed ( Symbol "coll", Symbol "index", Symbol "not-found" ) <| Callable.toArityFunction arity3
        , doc = Just """Returns the value at the index. get returns nil if index out of
bounds, nth throws an exception unless not-found is supplied. nth also works for strings."""
    }


vec : Callable io
vec =
    let
        arity1 collVal =
            case collVal of
                Vector _ ->
                    collVal |> Const |> Ok

                _ ->
                    collVal
                        |> Value.toSeq
                        |> Result.map (Value.vectorFromLocatedList >> Const)
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction arity1
        , doc = Just "Creates a new vector containing the contents of coll."
    }


vector : Callable io
vector =
    let
        arity0 signature =
            Value.vectorFromList signature.rest |> Const |> Ok
    in
    { emptyCallable
        | arity0 =
            Just <|
                Variadic { argNames = (), restArgName = Symbol "xs" } <|
                    Callable.toArityFunction arity0
        , doc = Just "Creates a new vector containing xs."
    }


prelude : String
prelude =
    """
(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f, has the same effects,
   if any, and returns the opposite truth value."
  [f]
  (fn [& args] (not (apply f args))))

(defn identity
  "Returns its argument."
  [a]
  a)

(defn comp
  "Takes a set of functions and returns a fn that is the composition
   of those fns.  The returned fn takes a variable number of args,
   applies the rightmost of fns to the args, the next
   fn (right-to-left) to the result, etc."
  [& fns]
  (reduce
    (fn [a e] (fn [& args] (a (apply e args))))
    identity
    fns))

(defn last
  "Return the last item in coll, in linear time."
  [coll]
  (if (next coll)
    (last (rest coll))
    (first coll)))

(defn next
  "Returns a seq of the items after the first. Calls seq on its argument. If there are no more items, returns nil."
  [coll]
  (seq (rest coll)))

(defn reverse
  "Returns a seq of the items in coll in reverse order."
  [coll]
  (reduce (fn [a e] (cons e a)) (list) coll))

(defn concat
  "Returns a seq representing the concatenation of the elements in the supplied colls."
  [& colls]
  (when (seq colls)
   (let [coll (first colls)]
     (reduce
      (fn [a e] (cons e a))
      (apply concat (rest colls))
      (reverse coll)))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of from-coll conjoined."
  [to from]
  (reduce conj to from))

(defn map
  "Returns a list consisting of the result of applying f to
   first item of coll, followed by applying f to the
   second item in coll, until coll
   exhausted."
  [f coll]
  (if (seq coll)
    (cons (f (first coll)) (map f (rest coll)))
    (list)))

(defn -map-indexed [i f coll]
  (if (seq coll)
    (cons (f i (first coll)) (-map-indexed (inc i) f (rest coll)))
    (list)))

(defn map-indexed
  "Returns a list consisting of the result of applying f to 0
   and the first item of coll, followed by applying f to 1 and the second
   item in coll, etc, until coll is exhausted. Thus function f should
   accept 2 arguments, index and item."
  [f coll]
  (-map-indexed 0 f coll))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
   to f and coll. Thus function f should return a collection."
  [f coll]
  (if (seq coll)
    (concat (f (first coll)) (mapcat f (rest coll)))
    (list)))

(defn filter
  "Returns a list of the items in coll for which (pred item) returns logical true."
  [pred coll]
  (if (seq coll)
    (let [el (first coll)]
      (if (pred el)
        (cons el (filter pred (rest coll)))
        (filter pred (rest coll))))
    (list)))

(defn remove
  "Returns a list of the items in coll for which (pred item) returns logical false."
  [pred coll]
  (filter (complement pred) coll))

(defn drop
  "Returns a list of all but the first n items in coll."
  [n coll]
  (if (and (seq coll) (pos? n))
    (drop (dec n) (rest coll))
    (or (seq coll) (list))))

(defn take
  "Returns a list of the first n items in coll, or all items if there are fewer than n."
  [n coll]
  (if (and (seq coll) (pos? n))
    (cons (first coll) (take (dec n) (rest coll)))
    (list)))

(defn drop-while
  "Returns a list of the items in coll starting from the first item for which (pred item) returns logical false."
  [pred coll]
  (if (and (seq coll) (pred (first coll)))
    (drop-while pred (rest coll))
    (or (seq coll) (list))))

(defn take-while
  "Returns a list of successive items from coll while (pred item) returns logical true."
  [pred coll]
  (if (and (seq coll) (pred (first coll)))
    (cons (first coll) (take-while pred (rest coll)))
    (list)))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
   returns the result of applying f to the first 2 items in coll, then
   applying f to that result and the 3rd item, etc. If coll contains no
   items, f must accept no arguments as well, and reduce returns the
   result of calling f with no arguments.  If coll has only 1 item, it
   is returned and f is not called.  If val is supplied, returns the
   result of applying f to val and the first item in coll, then
   applying f to that result and the 2nd item, etc. If coll contains no
   items, returns val and f is not called."
  ([f coll]
   (reduce f (f) coll))
  ([f init coll]
   (if (seq coll)
     (reduce f (f init (first coll)) (rest coll))
     init)))

(defn reduce-kv
  "Reduces a map. f should be a function of 3
   arguments. Returns the result of applying f to init, the first key
   and the first value in coll, then applying f to that result and the
   2nd key and value, etc. If coll contains no entries, returns init
   and f is not called."
  ([f coll]
   (reduce-kv f (f) coll))
  ([f init coll]
   (if (seq coll)
     (reduce-kv f (f init (key (first coll)) (val (first coll))) (rest coll))
     init)))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else false."
  [pred coll]
  (if (seq coll)
    (if (pred (first coll))
      (every? pred (rest coll))
      false)
    true))

(defn not-every?
  "Returns false if (pred x) is logical true for every x in coll, else true."
  [pred coll]
  (not (every? pred coll)))

(defn not-any?
  "Returns false if (pred x) is logical true for any x in coll, else true."
  [pred coll]
  (not (some pred coll)))

(defn repeat
  "Returns a list of length n of xs."
  [n x]
  (if (pos? n)
    (cons x (repeat (dec n) x))
    (list)))

(defn pos?
  "Returns true if x is greater than zero, else false."
  [x]
  (< 0 x))

(defn neg?
  "Returns true if x is less than zero, else false."
  [x]
  (< x 0))

(defn zero?
  "Returns true if x is zero, else false."
  [x]
  (= x 0))

(defn inc
  "Returns a number one greater than num."
  [x]
  (+ x 1))

(defn dec
  "Returns a number one less than num."
  [x]
  (- x 1))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  [num div]
  (let [m (rem num div)]
    (if (or (zero? m) (= (pos? num) (pos? div)))
      m
      (+ m div))))

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer."
  [n]
  (if (integer? n)
    (zero? (rem n 2))
    (throw (Exception. (str "Argument must be an integer: " n)))))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer."
  [n]
  (not (even? n)))

(defn get-in
  "Returns the value in a nested associative structure,
   where ks is a sequence of keys. Returns nil if the key
   is not present, or the not-found value if supplied."
  ([m ks]
   (reduce get m ks))
  ([m ks not-found]
   (reduce #(get %1 %2 not-found) m ks)))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
   sequence of keys and v is the new value and returns a new nested structure.
   If any levels do not exist, hash-maps will be created."
  ([m ks v]
   (let [k (first ks)
         ks (rest ks)]
     (if (seq ks)
       (assoc m k (assoc-in (get m k) ks v))
       (assoc m k v)))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
   else nil.  One common idiom is to use a set as pred, for example
   this will return :fred if :fred is in the sequence, otherwise nil:
   (some #{:fred} coll)"
  [pred coll]
  (if (seq coll)
    (let [x (first coll)
          ret (pred x)]
     (if ret
       ret
       (some pred (rest coll))))))

(defn update
  "'Updates' a value in an associative structure, where k is a
   key and f is a function that will take the old value
   and any supplied args and return the new value, and returns a new
   structure.  If the key does not exist, nil is passed as the old value."
  ([m k f & args]
   (assoc m k (apply f (get m k) args))))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
   sequence of keys and f is a function that will take the old value
   and any supplied args and return the new value, and returns a new
   nested structure.  If any levels do not exist, hash-maps will be
   created."
  ([m ks f & args]
   (assoc-in m ks (apply f (get-in m ks) args))))

(defn dedupe
  "Returns a list removing consecutive duplicates in coll."
  [coll]
  (if (seq coll)
    (let [el (first coll)
          rst (drop-while #(= el %) (rest coll))]
      (cons el (dedupe rst)))
    ()))

(defn -distinct
  [seen coll]
  (if (seq coll)
    (let [el (first coll)]
      (if (contains? seen el)
        (-distinct seen (rest coll))
        (cons el (-distinct (conj seen el) (rest coll)))))
    ()))

(defn distinct
  "Returns a list of the elements of coll with duplicates removed."
  [coll]
  (-distinct #{} coll))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
   a nil first argument to f with the supplied value x. Higher arity
   versions can replace arguments in the second and third
   positions (y, z). Note that the function f can take any number of
   arguments, not just the one(s) being nil-patched."
  [f default]
  (fn [& args]
    (apply f (if (= nil (first args)) default (first args)) (rest args))))

(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
   Please use the idiom (seq x) rather than (not (empty? x))"
  [coll]
  (not (seq coll)))

(defn update-vals
  "Given a map m and a function f of 1-argument, returns a new map where the keys of m
   are mapped to result of applying f to the corresponding values of m."
  [m f]
  (reduce-kv (fn [a k v] (assoc a k (f v))) {} m))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  [x]
  (fn [& _args] x))

(defn nil?
  "Returns true if x is nil, false otherwise."
  [x]
  (= nil x))

(defn true?
  "Returns true if x is true, false otherwise."
  [x]
  (= true x))

(defn false?
  "Returns true if x is false, false otherwise."
  [x]
  (= false x))

(defn some?
  "Returns true if x is not nil, false otherwise."
  [x]
  (not= nil x))

(defn distinct?
  "Returns true if no two of the arguments are ="
  [x & args]
  (= (inc (count args)) (count (into #{x} args))))

(defn max
  "Returns the greatest of the nums."
  [x & rst]
  (reduce #(if (< %1 %2) %2 %1) x rst))

(defn min
  "Returns the least of the nums."
  [x & rst]
  (reduce #(if (< %1 %2) %1 %2) x rst))

(defn abs
  "Returns the absolute value of a.
   If a is a double and zero => +0.0
   If a is a double and ##Inf or ##-Inf => ##Inf
   If a is a double and ##NaN => ##NaN"
  [x]
  (if (neg? x) (* -1 x) x))

(defn not-empty
  "If coll is empty, returns nil, else coll,"
  [coll]
  (when (seq coll) coll))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
   returns an list of n results of calling f."
  [n f]
  (if (< n 1)
    ()
    (cons (f) (repeatedly (dec n) f))))

(defn assert
  "Evaluates expr and throws an exception if it does not evaluate to logical true."
  ([x] (assert x "assertion error"))
  ([x message] (when-not x (throw (Exception. message)))))

(defn range
  "Returns a list of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1.
  When step is equal to 0, returns an empty list. When start is equal to end, returns empty list."
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step]
   (if (or (and (< start end) (pos? step))
           (and (< end start) (neg? step)))
    (cons start (range (+ step start) end step))
    ())))

(defn keep
  "Returns a list of the non-nil results of (f item). Note, this means false return values will be included."
  [f coll]
  (if (seq coll)
    (let [el (first coll)]
      (if-let [v (f el)]
        (cons v (keep f (rest coll)))
        (keep f (rest coll))))
    (list)))

(defn -keep-indexed [i f coll]
  (if (seq coll)
    (let [el (first coll)]
      (if-let [v (f i el)]
        (cons v (-keep-indexed (inc i) f (rest coll)))
        (-keep-indexed (inc i) f (rest coll))))
    (list)))

(defn keep-indexed
  "Returns a list of the non-nil results of (f index item). Note, this means false return values will be included."
  [f coll]
  (-keep-indexed 0 f coll))
"""
