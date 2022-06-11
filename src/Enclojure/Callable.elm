module Enclojure.Callable exposing
    ( Callable, new
    , fixedArity, variadicArity, setArity0, setArity1, setArity2, setArity3
    , signatures
    , toArityFunction
    )

{-| Helper functions for defining Enclojure callables.


# Getting started

@docs Callable, new


# Fleshing out your callable

Unlike Elm, one Enclojure function can have more than one "arity": the number of arguments it accepts. Arities can be
fixed (positional arguments only) or variadic (some or no positional arguments + a list of remaining arguments).

@docs fixedArity, variadicArity, setArity0, setArity1, setArity2, setArity3


# Misc

@docs signatures

-}

import Enclojure.Common exposing (Arity(..), Continuation, Env, Exception(..), IO, Step, Thunk(..), Value(..))
import Enclojure.Located exposing (Located(..))
import Enclojure.Value exposing (inspect)


{-| Represents a callable (anonymous function).
-}
type alias Callable io =
    Enclojure.Common.Callable io


{-| Creates a new empty callable.
-}
new : Callable io
new =
    { doc = Nothing
    , arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }


{-| Converts a simplified arity to full arity.

Most Enclojure callables don’t need to modify the environment or to access the continuation.
Thus, it's easier to define them as functions of `a -> Result Exception (IO io)`.

-}
toArityFunction : (a -> Result Exception (IO io)) -> (a -> Env io -> Continuation io -> Step io)
toArityFunction fn =
    \v env k ->
        ( fn v
            |> Result.map (\io -> ( io, env ))
            |> Result.mapError (\(Exception msg _) -> ( Exception msg env.stack, env ))
        , Just (Thunk k)
        )


{-| Overwrite the arity 0 (no positional arguments) on a callable.
-}
setArity0 : Enclojure.Common.Arity io () -> Callable io -> Callable io
setArity0 arity callable =
    { callable | arity0 = Just arity }


{-| Overwrite the arity 1 (one positional argument) on a callable
-}
setArity1 : Enclojure.Common.Arity io (Value io) -> Callable io -> Callable io
setArity1 arity callable =
    { callable | arity1 = Just arity }


{-| Overwrite the arity 2 (two positional arguments) on a callable
-}
setArity2 : Enclojure.Common.Arity io ( Value io, Value io ) -> Callable io -> Callable io
setArity2 arity callable =
    { callable | arity2 = Just arity }


{-| Overwrite the arity 3 (three positional arguments) on a callable
-}
setArity3 : Enclojure.Common.Arity io ( Value io, Value io, Value io ) -> Callable io -> Callable io
setArity3 arity callable =
    { callable | arity3 = Just arity }


{-| Build an arity that accepts positional args `a` and doesn’t accept any varargs.
-}
fixedArity : a -> (a -> Result Exception (IO io)) -> Enclojure.Common.Arity io a
fixedArity signature fn =
    Enclojure.Common.Fixed signature <| toArityFunction fn


{-| Build an arity that accepts positional args `a` and varargs.
-}
variadicArity : { argNames : a, restArgName : Value io } -> ({ args : a, rest : List (Value io) } -> Result Exception (IO io)) -> Enclojure.Common.Arity io a
variadicArity signature fn =
    Enclojure.Common.Variadic signature <| toArityFunction fn


{-| Returns the list of signatures of a given callable.
-}
signatures : Callable io -> List (List String)
signatures callable =
    [ callable.arity0 |> Maybe.map (always [])
    , callable.arity1
        |> Maybe.map
            (\arity ->
                case arity of
                    Fixed v _ ->
                        [ inspect v ]

                    Variadic { argNames, restArgName } _ ->
                        [ inspect argNames, "&", inspect restArgName ]
            )
    , callable.arity2
        |> Maybe.map
            (\arity ->
                case arity of
                    Fixed ( a, b ) _ ->
                        [ inspect a, inspect b ]

                    Variadic { argNames, restArgName } _ ->
                        let
                            ( a, b ) =
                                argNames
                        in
                        [ inspect a, inspect b, "& " ++ inspect restArgName ]
            )
    , callable.arity3
        |> Maybe.map
            (\arity ->
                case arity of
                    Fixed ( a, b, c ) _ ->
                        [ inspect a, inspect b, inspect c ]

                    Variadic { argNames, restArgName } _ ->
                        let
                            ( a, b, c ) =
                                argNames
                        in
                        [ inspect a, inspect b, inspect c, "&", inspect restArgName ]
            )
    ]
        |> List.filterMap identity
