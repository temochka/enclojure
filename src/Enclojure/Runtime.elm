module Enclojure.Runtime exposing
    ( emptyEnv, bindGlobal, bindLexical, fetchGlobal, fetchLexical, setCurrentStackFrameLocation
    , prettyTrace, throw
    , const, sideEffect
    , addAtom, deref, resetAtom
    , apply, terminate
    )

{-| Advanced functions for working with Enclojure runtime.


# Environment

@docs emptyEnv, bindGlobal, bindLexical, fetchGlobal, fetchLexical, setCurrentStackFrameLocation


# Exceptions

@docs prettyTrace, throw


# I/O

@docs const, sideEffect


# References

@docs addAtom, deref, resetAtom


# Misc

@docs apply, terminate

-}

import Dict
import Enclojure.Callable as Callable
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
        , Step
        , Thunk(..)
        , Value(..)
        )
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Reader
import Enclojure.Value as Value
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet exposing (ValueSet)


emptyCallable : Callable io
emptyCallable =
    Callable.new


{-| Returns an empty environment
-}
emptyEnv : Env io
emptyEnv =
    { globalScope = Dict.empty
    , lexicalScope = Dict.empty
    , atoms = Dict.empty
    , atomIdGenerator = 0
    , stack = [ { name = "user", location = Located.Unknown } ]
    }


{-| Binds a given `String` name to `Value io` in the lexical scope of `Env io`
-}
bindLexical : String -> Value io -> Env io -> Env io
bindLexical key value env =
    { env | lexicalScope = Dict.insert key value env.lexicalScope }


{-| Binds a given `String` name to `Value io` in the global scope of `Env io`.
-}
bindGlobal : String -> Value io -> Env io -> Env io
bindGlobal key value env =
    { env | globalScope = Dict.insert key value env.globalScope }


{-| Adds a new atom containing a provided value to the environment. Returns a tuple of the updated environment and the
generated atom id.
-}
addAtom : Value io -> Env io -> ( Env io, Int )
addAtom val env =
    let
        atomId =
            env.atomIdGenerator

        newEnv =
            { env
                | atoms = Dict.insert atomId val env.atoms
                , atomIdGenerator = atomId + 1
            }
    in
    ( newEnv, atomId )


{-| Fetches a value by name from the global scope
-}
fetchGlobal : String -> Env io -> Maybe (Value io)
fetchGlobal name env =
    Dict.get name env.globalScope


{-| Fetches a value by name from the lexical scope
-}
fetchLexical : String -> Env io -> Maybe (Value io)
fetchLexical name env =
    Dict.get name env.lexicalScope


{-| Dereferences given `Ref io` in `Env io`.
-}
deref : Ref io -> Env io -> Value io
deref ref env =
    case ref of
        Var _ value ->
            value

        Atom atomId ->
            Dict.get atomId env.atoms |> Maybe.withDefault Nil


{-| Resets an atom identified by `Int` id to value `Value io` in the given environment `Env io`.
-}
resetAtom : Int -> Value io -> Env io -> Env io
resetAtom atomId val env =
    { env | atoms = Dict.insert atomId val env.atoms }


getFn : String -> Callable io
getFn key =
    let
        arity1 mapVal =
            arity2 ( mapVal, Nil )

        arity2 ( mapVal, default ) =
            (case mapVal of
                Map m ->
                    ValueMap.get (Keyword key) m |> Maybe.map Located.getValue

                Set s ->
                    if ValueSet.member (Keyword key) s then
                        Just (Keyword key)

                    else
                        Nothing

                Nil ->
                    Nothing

                _ ->
                    Just Nil
            )
                |> Maybe.withDefault default
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "coll") <| Callable.toArityFunction (arity1 >> Const >> Ok)
        , arity2 = Just <| Fixed ( Symbol "coll", Symbol "not-found" ) <| Callable.toArityFunction (arity2 >> Const >> Ok)
    }


setLookupFn : ValueSet io -> Callable io
setLookupFn set =
    let
        arity1 val =
            if ValueSet.member val set then
                val

            else
                Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "x") <| Callable.toArityFunction (arity1 >> Const >> Ok)
    }


mapLookupFn : ValueMap io -> Callable io
mapLookupFn map =
    let
        arity1 val =
            ValueMap.get val map |> Maybe.map Located.getValue |> Maybe.withDefault Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed (Symbol "key") <| Callable.toArityFunction (arity1 >> Const >> Ok)
    }


{-| Attempts to interpret the first argument as a function and the second argument as a list of its arguments.
-}
apply : Located (Value io) -> Located (Value io) -> Env io -> Continuation io -> Located (Step io)
apply ((Located fnLoc fnExpr) as fn) arg inputEnv inputK =
    let
        currentStack =
            inputEnv.stack
                |> List.head
                |> Maybe.map (\frame -> { frame | location = fnLoc } :: List.drop 1 inputEnv.stack)
                |> Maybe.withDefault inputEnv.stack

        k =
            \v kEnv -> inputK v { kEnv | stack = List.drop 1 kEnv.stack }
    in
    case fnExpr of
        Fn { name } callable ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = name |> Maybe.withDefault "fn"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (callable { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Keyword key ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = key
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (Common.toThunk (getFn key) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Map map ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = "Map"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (Common.toThunk (mapLookupFn map) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Set set ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = "Set"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (Common.toThunk (setLookupFn set) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        _ ->
            ( Err
                ( Value.exception (Value.inspectLocated fn ++ " is not a valid callable.") |> throw inputEnv
                , inputEnv
                )
            , Just (Thunk k)
            )
                |> Located fnLoc


setStackTrace : List Common.StackFrame -> Exception -> Exception
setStackTrace stack (Exception msg _) =
    Exception msg stack


{-| Overwrites the location of the current stack frame in a given environment.
-}
setCurrentStackFrameLocation : Located.Location -> Env io -> Env io
setCurrentStackFrameLocation location env =
    let
        newStack =
            env.stack
                |> List.head
                |> Maybe.map (\currentFrame -> { currentFrame | location = location } :: List.drop 1 env.stack)
                |> Maybe.withDefault env.stack
    in
    { env | stack = newStack }


{-| Return a "prettified" stack trace for an exception.
-}
prettyTrace : Exception -> List String
prettyTrace (Exception _ trace) =
    trace
        |> List.map
            (\frame ->
                frame.name
                    ++ (case frame.location of
                            Located.Unknown ->
                                ""

                            Located.Known { start } ->
                                ":" ++ (Tuple.first start |> String.fromInt)
                       )
            )


{-| Assigns the stack from the given environment to the exception.
-}
throw : Env io -> Exception -> Exception
throw env (Exception msg _) =
    Exception msg env.stack


{-| Indicates that the returned value is a side effect.
-}
sideEffect : io -> IO io
sideEffect =
    SideEffect


{-| Indicates that the returned value is a constant.
-}
const : Value io -> IO io
const =
    Const


{-| Returns a continuation that terminates the program.
-}
terminate : Continuation io
terminate (Located pos v) env =
    Located pos ( Ok ( Const v, env ), Nothing )
