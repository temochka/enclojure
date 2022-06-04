module Enclojure.Callable exposing
    ( Callable
    , fixedArity
    , new
    , setArity0
    , setArity1
    , setArity2
    , setArity3
    , toArityFunction
    , toThunk
    , variadicArity
    )

import Enclojure.Common exposing (Arity(..), Continuation, Env, Exception(..), IO, Step, Thunk(..), Value(..))
import Enclojure.Extra.Maybe
import Enclojure.Located as Located exposing (Located(..))


type alias Callable io =
    Enclojure.Common.Callable io


new : Callable io
new =
    { arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }


toArityFunction : (a -> Result Exception (IO io)) -> (a -> Env io -> Continuation io -> Step io)
toArityFunction fn =
    \v env k ->
        ( fn v
            |> Result.map (\io -> ( io, env ))
            |> Result.mapError (\(Exception msg _) -> ( Exception msg env.stack, env ))
        , Just (Thunk k)
        )


setArity0 : Enclojure.Common.Arity io () -> Callable io -> Callable io
setArity0 arity callable =
    { callable | arity0 = Just arity }


setArity1 : Enclojure.Common.Arity io (Value io) -> Callable io -> Callable io
setArity1 arity callable =
    { callable | arity1 = Just arity }


setArity2 : Enclojure.Common.Arity io ( Value io, Value io ) -> Callable io -> Callable io
setArity2 arity callable =
    { callable | arity2 = Just arity }


setArity3 : Enclojure.Common.Arity io ( Value io, Value io, Value io ) -> Callable io -> Callable io
setArity3 arity callable =
    { callable | arity3 = Just arity }


fixedArity : (a -> Result Exception (IO io)) -> Enclojure.Common.Arity io a
fixedArity fn =
    Enclojure.Common.Fixed <| toArityFunction fn


variadicArity : ({ args : a, rest : List (Value io) } -> Result Exception (IO io)) -> Enclojure.Common.Arity io a
variadicArity fn =
    Enclojure.Common.Variadic <| toArityFunction fn


toThunk : Callable io -> { self : Value io, k : Continuation io } -> Thunk io
toThunk callable { k } =
    Thunk
        (\(Located pos arg) env ->
            case arg of
                List args ->
                    dispatch callable (List.map Located.getValue args) env k
                        |> Located pos

                _ ->
                    Located pos ( Err ( Exception "Foo" env.stack, env ), Nothing )
        )


dispatch : Callable io -> List (Value io) -> Env io -> Continuation io -> Step io
dispatch callable args env k =
    case args of
        [] ->
            callable.arity0
                |> Maybe.map
                    (\arity0 ->
                        case arity0 of
                            Fixed fn ->
                                fn () env k

                            Variadic fn ->
                                fn { args = (), rest = [] } env k
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 0" env.stack, env ), Just (Thunk k) )

        [ a0 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity1
                            |> Maybe.map
                                (\arity1 ->
                                    case arity1 of
                                        Fixed fn ->
                                            fn a0 env k

                                        Variadic fn ->
                                            fn { args = a0, rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 1" env.stack, env ), Just (Thunk k) )

        [ a0, a1 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity2
                            |> Maybe.map
                                (\arity2 ->
                                    case arity2 of
                                        Fixed fn ->
                                            fn ( a0, a1 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 2" env.stack, env ), Just (Thunk k) )

        [ a0, a1, a2 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity3
                            |> Maybe.map
                                (\arity3 ->
                                    case arity3 of
                                        Fixed fn ->
                                            fn ( a0, a1, a2 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1, a2 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 3" env.stack, env ), Just (Thunk k) )

        a0 :: a1 :: a2 :: rest ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity3
                            |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest } env k)
                    )
                |> Maybe.withDefault
                    ( Err
                        ( Exception ("Invalid arity " ++ String.fromInt (List.length args)) env.stack
                        , env
                        )
                    , Nothing
                    )


extractVariadic : Maybe (Arity io a) -> Maybe ({ args : a, rest : List (Value io) } -> Env io -> Continuation io -> Step io)
extractVariadic arity =
    arity
        |> Maybe.andThen
            (\a ->
                case a of
                    Fixed _ ->
                        Nothing

                    Variadic fn ->
                        Just fn
            )
