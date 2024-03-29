module Enclojure exposing
    ( Env, init
    , Exception, evalPure
    , EvalResult(..), Step, eval, continueEval
    , getStepEnv, setStepEnv, getStepValue
    , Doc(..), FnInfo, documentation
    )

{-| Enclojure is a Clojure-like scripting language for Elm apps. Enclojure is experimental software and subject to breaking API changes.


# Initialize

@docs Env, init


# Pure programs

@docs Exception, evalPure


# Side-effecting programs

@docs EvalResult, Step, eval, continueEval


# Manipulating execution environment

@docs getStepEnv, setStepEnv, getStepValue


# Extracting documentation

@docs Doc, FnInfo, documentation

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Enclojure.Callable as Callable exposing (Callable)
import Enclojure.Common
    exposing
        ( Continuation
        , Env
        , Exception(..)
        , FnInfo
        , IO(..)
        , Number(..)
        , Ref(..)
        , Thunk(..)
        , Value(..)
        , toThunk
        )
import Enclojure.Extra.Maybe exposing (orElse)
import Enclojure.Lib as Lib
import Enclojure.Lib.String as LibString
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Reader
import Enclojure.Reader.Macros as Macros
import Enclojure.Runtime as Runtime
import Enclojure.Value as Value
import Enclojure.ValueMap as ValueMap exposing (ValueMap, ValueMapEntry)
import Enclojure.ValueSet as ValueSet exposing (ValueSet)
import Parser


{-| An Enclojure exception value.
-}
type alias Exception =
    Enclojure.Common.Exception


{-| An execution environment. Think of this as the current state of the interpreter.
-}
type alias Env io =
    Enclojure.Common.Env io


resolveSymbol : Env io -> String -> Result Exception (Value io)
resolveSymbol env symbol =
    Runtime.fetchLexical symbol env
        |> orElse (\_ -> Runtime.fetchGlobal symbol env)
        |> Result.fromMaybe (Value.exception ("Unknown symbol " ++ symbol) |> Runtime.throw env)


{-| Introduce a redundant closure to prevent closure shadowing via tail-call optimization
in functions with continuations. See: <https://github.com/elm/compiler/issues/2017>

Elm compiler doesn’t seem to be able to optimize this away.

-}
closureHack3 : a -> b -> c -> (a -> b -> c -> d) -> d
closureHack3 a b c f =
    f a b c


evalExpression : Located (Value io) -> Env io -> Continuation io -> Located (Enclojure.Common.Step io)
evalExpression mutableExpr mutableEnv mutableK =
    closureHack3
        mutableExpr
        mutableEnv
        mutableK
        (\(Located loc expr) inputEnv k ->
            let
                env =
                    Runtime.setCurrentStackFrameLocation loc inputEnv
            in
            case expr of
                Vector v ->
                    evalVector (Located loc v) env k

                Map m ->
                    evalMap (Located loc m) env k

                Set s ->
                    evalSet (Located loc s) env k

                Symbol s ->
                    case resolveSymbol env s of
                        Ok v ->
                            Located loc ( Ok ( Const v, env ), Just (Thunk k) )

                        Err e ->
                            Located loc ( Err ( e, env ), Just (Thunk k) )

                String s ->
                    Located loc ( Ok ( Const (String s), env ), Just (Thunk k) )

                Keyword s ->
                    Located loc ( Ok ( Const (Keyword s), env ), Just (Thunk k) )

                Number (Float _) ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Number (Int _) ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                MapEntry _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Nil ->
                    Located loc ( Ok ( Const Nil, env ), Just (Thunk k) )

                Bool _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Ref _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Regex _ _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Fn _ _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                List l ->
                    case l of
                        -- special forms
                        ((Located _ (Symbol name)) as fnExpr) :: argExprs ->
                            specialFormsByName
                                |> Dict.get name
                                |> Maybe.map (\form -> form.eval (Located loc argExprs) env k)
                                |> Maybe.withDefault (evalApply fnExpr (Located loc argExprs) env k)

                        -- apply
                        fnExpr :: argExprs ->
                            evalApply fnExpr (Located loc argExprs) env k

                        -- empty list ()
                        [] ->
                            Located loc ( Ok ( Const expr, env ), Just (Thunk k) )

                Throwable _ ->
                    Located loc ( Ok ( Const expr, env ), Just (Thunk k) )
        )


evalVector : Located (Array (Located (Value io))) -> Env io -> Continuation io -> Located (Enclojure.Common.Step io)
evalVector (Located loc vecV) env k =
    Located loc
        ( Ok ( Const (Vector Array.empty), env )
        , vecV
            |> Array.foldr
                (\e a ->
                    \(Located _ v) envNow ->
                        case v of
                            Vector array ->
                                evalExpression e
                                    envNow
                                    (\ret retEnv ->
                                        Located loc
                                            ( Ok ( Const (Vector (Array.push ret array)), retEnv )
                                            , Just (Thunk a)
                                            )
                                    )

                            _ ->
                                Located loc
                                    ( Err
                                        ( Value.exception "Interpreter error: vector evaluation yielded a non-vector"
                                            |> Runtime.throw envNow
                                        , envNow
                                        )
                                    , Just (Thunk a)
                                    )
                )
                k
            |> Thunk
            |> Just
        )


evalMap : Located (ValueMap io) -> Env io -> Continuation io -> Step io
evalMap (Located mapLoc map) env k =
    Located mapLoc
        ( Ok ( Const (Map ValueMap.empty), env )
        , map
            |> ValueMap.toList
            |> List.map (Located mapLoc)
            |> List.foldl
                (\e a ->
                    \(Located loc v) renv ->
                        case v of
                            Map m ->
                                evalMapEntry e
                                    renv
                                    (\(Located mapEntryLoc mapEntryV) mapEntryEnv ->
                                        case mapEntryV of
                                            MapEntry ( key, value ) ->
                                                Located mapEntryLoc
                                                    ( Ok
                                                        ( Const (Map (ValueMap.insert key value m))
                                                        , mapEntryEnv
                                                        )
                                                    , Just (Thunk a)
                                                    )

                                            _ ->
                                                Located loc
                                                    ( Value.exception "Interpreter error: Map entry evaluation yielded a non-map entry"
                                                        |> Runtime.throw mapEntryEnv
                                                        |> (\ex -> ( ex, mapEntryEnv ))
                                                        |> Err
                                                    , Just (Thunk a)
                                                    )
                                    )

                            _ ->
                                Located loc
                                    ( Value.exception "Interpreter error: Map evaluation yielded a non-map"
                                        |> Runtime.throw renv
                                        |> (\ex -> ( ex, renv ))
                                        |> Err
                                    , Just (Thunk a)
                                    )
                )
                k
            |> Thunk
            |> Just
        )


evalMapEntry : Located (ValueMapEntry io) -> Env io -> Continuation io -> Step io
evalMapEntry (Located loc ( key, value )) env k =
    evalExpression (Located.unknown key)
        env
        (\keyRet keyEnv ->
            evalExpression value
                keyEnv
                (\valRet valEnv ->
                    Located loc
                        ( Ok ( Const (MapEntry ( Located.getValue keyRet, valRet )), valEnv )
                        , Just (Thunk k)
                        )
                )
        )


evalSet : Located (ValueSet io) -> Env io -> Continuation io -> Step io
evalSet (Located setLoc set) env k =
    Located setLoc
        ( Ok ( Const (Set ValueSet.empty), env )
        , set
            |> ValueSet.toList
            |> List.map (Located setLoc)
            |> List.foldl
                (\e a ->
                    \(Located loc v) renv ->
                        case v of
                            Set s ->
                                evalExpression e
                                    renv
                                    (\(Located setEntryLoc setEntry) setEntryEnv ->
                                        Located setEntryLoc
                                            ( Ok
                                                ( Const (Set (ValueSet.insert setEntry s))
                                                , setEntryEnv
                                                )
                                            , Just (Thunk a)
                                            )
                                    )

                            _ ->
                                Located loc
                                    ( Value.exception "Interpreter error: Set evaluation yielded a non-set"
                                        |> Runtime.throw renv
                                        |> (\ex -> ( ex, renv ))
                                        |> Err
                                    , Just (Thunk a)
                                    )
                )
                k
            |> Thunk
            |> Just
        )


destructure : Located (Value io) -> Located (Value io) -> Result (Located Exception) (List ( String, Value io ))
destructure arg template =
    case template of
        Located _ (Symbol name) ->
            Ok [ ( name, Located.getValue arg ) ]

        Located loc (Map templateMap) ->
            arg
                |> Located.getValue
                |> Value.toMap
                |> Result.fromMaybe (Located loc (Exception "type error: destructured value is not associative" []))
                |> Result.andThen
                    (\map ->
                        let
                            keywordKeys =
                                templateMap
                                    |> ValueMap.get (Keyword "keys")
                                    |> Maybe.map Located.getValue
                                    |> Maybe.withDefault (List [])
                                    |> Value.trySequenceOf Value.trySymbol
                                    |> Result.fromMaybe (Located loc (Exception "type error: :keys must be a vector of symbols" []))
                                    |> Result.map
                                        (\keys ->
                                            keys
                                                |> List.map
                                                    (\key ->
                                                        ( key
                                                        , ValueMap.get (Keyword key) map
                                                            |> Maybe.map Located.getValue
                                                            |> Maybe.withDefault Nil
                                                        )
                                                    )
                                        )

                            stringKeys =
                                templateMap
                                    |> ValueMap.get (Keyword "strs")
                                    |> Maybe.map Located.getValue
                                    |> Maybe.withDefault (List [])
                                    |> Value.trySequenceOf Value.trySymbol
                                    |> Result.fromMaybe (Located loc (Exception "type error: :strs must be a vector of symbols" []))
                                    |> Result.map
                                        (\keys ->
                                            keys
                                                |> List.map
                                                    (\key ->
                                                        ( key
                                                        , ValueMap.get (String key) map
                                                            |> Maybe.map Located.getValue
                                                            |> Maybe.withDefault Nil
                                                        )
                                                    )
                                        )

                            otherKeys =
                                templateMap
                                    |> ValueMap.remove (Keyword "keys")
                                    |> ValueMap.remove (Keyword "strs")
                                    |> ValueMap.remove (Keyword "or")
                                    |> ValueMap.toList
                                    |> List.foldr
                                        (\( keyTemplate, Located _ key ) allBindingsResult ->
                                            allBindingsResult
                                                |> Result.andThen
                                                    (\allBindings ->
                                                        destructure
                                                            (ValueMap.get key map
                                                                |> Maybe.withDefault (Located loc Nil)
                                                            )
                                                            (Located loc keyTemplate)
                                                            |> Result.map ((++) allBindings)
                                                    )
                                        )
                                        (Ok [])

                            defaults =
                                if ValueMap.member (Keyword "or") templateMap then
                                    Err (Located loc (Exception ":or is not supported" []))

                                else
                                    Ok []
                        in
                        Result.map4 (\a b c d -> a ++ b ++ c ++ d)
                            defaults
                            keywordKeys
                            stringKeys
                            otherKeys
                    )

        Located loc (Vector aliasedVector) ->
            let
                asKeyword =
                    Array.get (Array.length aliasedVector - 2) aliasedVector
                        |> Maybe.andThen (Located.getValue >> Value.tryKeyword)

                asTemplate =
                    Array.get (Array.length aliasedVector - 1) aliasedVector
                        |> Maybe.andThen (Located.getValue >> Value.trySymbol)

                ( vec, asBindings ) =
                    Maybe.map2
                        (\_ tmpl ->
                            ( Array.slice 0 -2 aliasedVector, [ ( tmpl, Located.getValue arg ) ] )
                        )
                        asKeyword
                        asTemplate
                        |> Maybe.withDefault ( aliasedVector, [] )
            in
            case Array.toList vec of
                [] ->
                    Ok asBindings

                [ Located _ (Symbol "&"), nextTemplate ] ->
                    arg
                        |> Located.getValue
                        |> Value.toSeq
                        |> Result.mapError (Located loc)
                        |> Result.andThen
                            (\seq ->
                                destructure (Located.sameAs arg (List seq)) nextTemplate
                            )
                        |> Result.map ((++) asBindings)

                nextTemplate :: rest ->
                    arg
                        |> Located.getValue
                        |> Value.toSeq
                        |> Result.mapError (Located loc)
                        |> Result.andThen
                            (\argAsList ->
                                let
                                    newArg =
                                        argAsList |> List.head |> Maybe.withDefault (Located.unknown Nil)
                                in
                                destructure newArg nextTemplate
                                    |> Result.andThen
                                        (\boundArgs ->
                                            destructure
                                                (argAsList
                                                    |> List.tail
                                                    |> Maybe.map List
                                                    |> Maybe.withDefault Nil
                                                    |> Located.unknown
                                                )
                                                (Located loc (Vector (Array.fromList rest)))
                                                |> Result.map ((++) asBindings)
                                                |> Result.map ((++) boundArgs)
                                        )
                            )

        _ ->
            Err (Located.sameAs arg (Exception "Parsing error: arguments didn't match the function definition" []))


mapArgs : List (Located (Value io)) -> List (Located (Value io)) -> Result (Located Exception) (List ( String, Value io ))
mapArgs args bindings =
    case ( args, bindings ) of
        ( _, (Located _ (Symbol "&")) :: (Located _ (Symbol name)) :: [] ) ->
            Ok [ ( name, List args ) ]

        ( _, (Located loc (Symbol "&")) :: [] ) ->
            Err (Located loc (Exception "Parsing error: no symbol after &" []))

        ( (Located loc _) :: _, [] ) ->
            Err (Located loc (Exception "Argument error: Too many arguments" []))

        ( [], (Located loc (Symbol _)) :: _ ) ->
            Err (Located loc (Exception "Argument error: Too few arguments" []))

        ( arg :: restArgs, template :: restBindings ) ->
            destructure arg template
                |> Result.andThen
                    (\destructuredArgs ->
                        mapArgs restArgs restBindings |> Result.map (\b -> destructuredArgs ++ b)
                    )

        ( _, (Located loc _) :: _ ) ->
            Err (Located loc (Exception "Parsing error: arguments didn't match the function definition" []))

        ( [], [] ) ->
            Ok []


bindArgs : Located (Value io) -> List (Located (Value io)) -> Env io -> Result (Located Exception) (Env io)
bindArgs (Located loc argsExpr) bindings env =
    case argsExpr of
        List args ->
            mapArgs args bindings
                |> Result.map
                    (List.foldl (\( k, v ) aEnv -> Runtime.bindLexical k v aEnv) env)

        _ ->
            Err (Located loc (Exception "Interpreter error: applied arguments are not a list" []))


mapBindingsToBodies : List (Located (Value io)) -> Result (Located Exception) (List ( List (Located (Value io)), Located (Value io) ))
mapBindingsToBodies signatures =
    case signatures of
        (Located _ (List ((Located loc (Vector argBindings)) :: body))) :: rest ->
            mapBindingsToBodies rest |> Result.map (\r -> ( Array.toList argBindings, wrapInDo (Located loc body) ) :: r)

        (Located loc _) :: _ ->
            Err (Located loc (Exception "Parsing error: malformed function arity" []))

        [] ->
            Ok []


listLocate : (a -> Maybe b) -> List a -> Maybe b
listLocate pFn l =
    case l of
        [] ->
            Nothing

        e :: rest ->
            case pFn e of
                Just v ->
                    Just v

                Nothing ->
                    listLocate pFn rest


exctractFnNameAndDoc : List (Located (Value io)) -> ( Maybe String, Maybe String, List (Located (Value io)) )
exctractFnNameAndDoc exprs =
    case exprs of
        (Located _ (Symbol name)) :: (Located _ (String doc)) :: rest ->
            ( Just name, Just doc, rest )

        (Located _ (Symbol name)) :: rest ->
            ( Just name, Nothing, rest )

        (Located _ (String doc)) :: rest ->
            ( Nothing, Just doc, rest )

        _ ->
            ( Nothing, Nothing, exprs )


evalFn : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalFn (Located loc exprs) fnEnv k =
    let
        ( name, doc, arity ) =
            exctractFnNameAndDoc exprs
    in
    case arity of
        (Located _ (Vector argBindings)) :: body ->
            Located loc
                ( Ok
                    ( Fn
                        { name = name
                        , doc = doc
                        , signatures = [ argBindings |> Array.toList |> List.map (Located.getValue >> Value.inspect) ]
                        }
                        (\fn ->
                            (\args callsiteEnv ->
                                let
                                    callEnvResult =
                                        { callsiteEnv | lexicalScope = fnEnv.lexicalScope }
                                            |> (name
                                                    |> Maybe.map (\n -> Runtime.bindLexical n fn.self)
                                                    |> Maybe.withDefault identity
                                               )
                                            |> Runtime.bindLexical "recur" fn.self
                                            |> bindArgs args (Array.toList argBindings)
                                in
                                case callEnvResult of
                                    Ok callEnv ->
                                        evalExpression (wrapInDo (Located loc body))
                                            callEnv
                                            (scrubLocalEnv callsiteEnv fn.k)

                                    Err e ->
                                        Located.sameAs e ( Err ( Located.getValue e, fnEnv ), Just (Thunk fn.k) )
                            )
                                |> Thunk
                        )
                        |> Const
                    , fnEnv
                    )
                , Just (Thunk k)
                )

        signatures ->
            let
                fnSignatures =
                    signatures
                        |> List.filterMap
                            (Located.getValue
                                >> Value.tryList
                                >> Maybe.andThen List.head
                                >> Maybe.map Located.getValue
                                >> Maybe.andThen Value.tryVector
                                >> Maybe.map Array.toList
                                >> Maybe.map (List.map (Located.getValue >> Value.inspect))
                            )
            in
            Located loc
                ( ( (\fn ->
                        (\args callsiteEnv ->
                            let
                                callEnvBodyResult =
                                    { callsiteEnv | lexicalScope = fnEnv.lexicalScope }
                                        |> (name
                                                |> Maybe.map (\n -> Runtime.bindLexical n fn.self)
                                                |> Maybe.withDefault identity
                                           )
                                        |> Runtime.bindLexical "recur" fn.self
                                        |> (\env ->
                                                mapBindingsToBodies signatures
                                                    |> Result.andThen
                                                        (listLocate
                                                            (\( bindings, body ) ->
                                                                bindArgs args bindings env
                                                                    |> Result.toMaybe
                                                                    |> Maybe.map (\e -> ( e, body ))
                                                            )
                                                            >> Result.fromMaybe
                                                                (Located loc
                                                                    (Exception
                                                                        "Argument error: no matching arity"
                                                                        []
                                                                    )
                                                                )
                                                        )
                                           )
                            in
                            case callEnvBodyResult of
                                Ok ( callEnv, fnBody ) ->
                                    evalExpression fnBody
                                        callEnv
                                        (scrubLocalEnv callsiteEnv fn.k)

                                Err e ->
                                    Located.sameAs e ( Err ( Located.getValue e, callsiteEnv ), Just (Thunk fn.k) )
                        )
                            |> Thunk
                    )
                        |> Fn { name = name, signatures = fnSignatures, doc = Nothing }
                        |> Const
                  , fnEnv
                  )
                    |> Ok
                , Just (Thunk k)
                )


scrubLocalEnv : Env io -> Continuation io -> Continuation io
scrubLocalEnv priorEnv k =
    \v env ->
        Located.sameAs v ( Ok ( Const <| Located.getValue v, { env | lexicalScope = priorEnv.lexicalScope } ), Just (Thunk k) )


evalLet : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalLet (Located loc body) env k =
    let
        parseBindings bindings =
            case bindings of
                template :: value :: rest ->
                    parseBindings rest
                        |> Result.map (\r -> ( template, value ) :: r)

                [] ->
                    Ok []

                _ ->
                    Value.exception "Syntax error: let received an uneven number of binding pairs"
                        |> Runtime.throw env
                        |> Located loc
                        |> Err
    in
    case body of
        (Located bodyLoc (Vector bindings)) :: doBody ->
            Located loc
                ( Ok ( Const Nil, env )
                , Just
                    (bindings
                        |> Array.toList
                        |> parseBindings
                        |> Result.map
                            (List.foldr
                                (\( template, e ) a ->
                                    \_ eenv ->
                                        evalExpression e
                                            eenv
                                            (\ret retEnv ->
                                                Located loc
                                                    ( destructure ret template
                                                        |> Result.mapError (\ex -> ( Located.getValue ex, retEnv ))
                                                        |> Result.andThen
                                                            (\destructuredBindings ->
                                                                Ok <|
                                                                    ( Const Nil
                                                                    , destructuredBindings
                                                                        |> List.foldl (\( name, v ) -> Runtime.bindLexical name v) retEnv
                                                                    )
                                                            )
                                                    , Just (Thunk a)
                                                    )
                                            )
                                )
                                (\_ renv ->
                                    wrapInDo (Located bodyLoc doBody)
                                        |> (\b -> evalExpression b renv (scrubLocalEnv env k))
                                )
                            )
                        |> Result.map Thunk
                        |> (\r ->
                                case r of
                                    Ok v ->
                                        v

                                    Err e ->
                                        Thunk
                                            (\_ errEnv ->
                                                Located.sameAs e
                                                    ( Err ( Located.getValue e, errEnv )
                                                    , Just (Thunk (scrubLocalEnv env k))
                                                    )
                                            )
                           )
                    )
                )

        _ ->
            Located loc
                ( Err
                    ( Value.exception "Syntax error: let expects a vector of bindings" |> Runtime.throw env
                    , env
                    )
                , Just (Thunk k)
                )


evalApply : Located (Value io) -> Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalApply fnExpr (Located loc argExprs) env k =
    evalExpression fnExpr
        env
        (\fn fnEnv ->
            Located loc
                ( Ok ( Const (List []), fnEnv )
                , Just
                    (argExprs
                        |> List.foldr
                            (\e a ->
                                \(Located _ args) argsEnv ->
                                    evalExpression e
                                        argsEnv
                                        (\arg argEnv ->
                                            case args of
                                                List ea ->
                                                    a (Located loc (List (ea ++ [ arg ]))) argEnv

                                                _ ->
                                                    Located loc
                                                        ( Value.exception "Impossible interpreter state: list evaluation yielded a non-list"
                                                            |> Runtime.throw argEnv
                                                            |> (\ex -> ( ex, argEnv ))
                                                            |> Err
                                                        , Just (Thunk a)
                                                        )
                                        )
                            )
                            (\args argsEnv -> Runtime.apply fn args argsEnv k)
                        |> Thunk
                    )
                )
        )


evalQuote : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalQuote (Located loc exprs) env k =
    case exprs of
        [ arg ] ->
            Located.sameAs arg
                ( Ok ( Const <| Located.getValue arg, env )
                , Just (Thunk k)
                )

        _ ->
            Located loc
                ( Err
                    ( Value.exception
                        ("Argument error: Wrong number of arguments ("
                            ++ String.fromInt (List.length exprs)
                            ++ ") passed to quote"
                        )
                        |> Runtime.throw env
                    , env
                    )
                , Just (Thunk k)
                )


evalDo : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalDo (Located loc exprs) env k =
    Located loc
        ( Ok ( Const Nil, env )
        , Just
            (Thunk
                (exprs
                    |> List.foldr
                        (\e a -> \_ eenv -> evalExpression e eenv a)
                        (\r renv -> Located.sameAs r ( Ok ( Const <| Located.getValue r, renv ), Just (Thunk k) ))
                )
            )
        )


evalIf : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalIf (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ :: _ ->
            Located loc ( Err ( Value.exception "an if with too many forms" |> Runtime.throw env, env ), Just (Thunk k) )

        [ eIf, eThen, eElse ] ->
            evalExpression eIf
                env
                (\ifRet ifEnv ->
                    if Value.isTruthy (Located.getValue ifRet) then
                        evalExpression eThen ifEnv k

                    else
                        evalExpression eElse ifEnv k
                )

        [ eIf, eThen ] ->
            evalExpression eIf
                env
                (\ifRet ifEnv ->
                    if Value.isTruthy (Located.getValue ifRet) then
                        evalExpression eThen ifEnv k

                    else
                        Located loc ( Ok ( Const Nil, ifEnv ), Just (Thunk k) )
                )

        [ _ ] ->
            Located loc ( Err ( Value.exception "an if without then" |> Runtime.throw env, env ), Just (Thunk k) )

        [] ->
            Located loc ( Err ( Value.exception "an empty if" |> Runtime.throw env, env ), Just (Thunk k) )


evalDef : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io
evalDef (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ ->
            Located loc
                ( Err ( Value.exception "too many arguments to def" |> Runtime.throw env, env )
                , Just (Thunk k)
                )

        [ Located _ (Symbol name), e ] ->
            evalExpression e
                env
                (\eret eenv ->
                    Located loc
                        ( Ok
                            ( Const (Ref (Var name (Located.getValue eret)))
                            , Runtime.bindGlobal name (Located.getValue eret) eenv
                            )
                        , Just (Thunk k)
                        )
                )

        [ _, _ ] ->
            Located loc
                ( Err
                    ( Value.exception "def accepts a symbol and an expression" |> Runtime.throw env
                    , env
                    )
                , Just (Thunk k)
                )

        [ Located _ (Symbol name) ] ->
            Located loc ( Err ( Value.exception ("empty def " ++ name) |> Runtime.throw env, env ), Just (Thunk k) )

        [ _ ] ->
            Located loc
                ( Err ( Value.exception "def expects a symbol as its first argument" |> Runtime.throw env, env )
                , Just (Thunk k)
                )

        [] ->
            Located loc ( Err ( Value.exception "no arguments to def" |> Runtime.throw env, env ), Just (Thunk k) )


evalCallable : Callable io
evalCallable =
    let
        arity1 form env1 k =
            evalExpression (Located.unknown form) env1 k
                |> Located.getValue
    in
    Callable.new
        |> Callable.setArity1 (Enclojure.Common.Fixed (Value.symbol "str") arity1)
        |> Callable.setDoc (Just "Evaluates the form data structure and returns the result.")


wrapInDo : Located (List (Located (Value io))) -> Located (Value io)
wrapInDo (Located loc vs) =
    Located loc (List (Located loc (Symbol "do") :: vs))


prelude : Result Exception (List (Located (Value io)))
prelude =
    Reader.readString Lib.prelude
        |> Result.mapError (Runtime.throw defaultEnv)


defaultEnv : Env io
defaultEnv =
    Runtime.emptyEnv
        |> Lib.init
        -- Eval is implemented in this namespace so lib cannot provide it.
        |> Runtime.bindGlobal "eval"
            (Fn
                { name = Just "eval"
                , doc = evalCallable.doc
                , signatures = Callable.signatures evalCallable
                }
                (toThunk evalCallable)
            )
        |> LibString.init


{-| Initialize a fresh Enclojure execution environment
-}
init : Env io
init =
    prelude
        |> Result.map (Located.unknown >> wrapInDo)
        |> Result.map (\program -> evalExpression program defaultEnv Runtime.terminate)
        |> Result.andThen (continueEvalPure { maxOps = Nothing })
        |> Result.map (\( _, env ) -> env)
        |> Result.withDefault defaultEnv


{-| Represents an unfinished computation that can be continued using `continueEval`.
-}
type alias Step io =
    Located (Enclojure.Common.Step io)


{-| Returned by `eval`

  - `Done value` - program completed successfully and returned `value`.
  - `Error exception` - program threw `exception`.
  - `Continue step` - program reached its `EvalOptions.maxOps` quota and can be resumed with continueEval (perhaps at the next animation frame).
  - `RunIO io toStep` - program returned a side effect `io` and expects the caller to handle the side effect and call `toStep` with the result.

-}
type EvalResult io
    = RunIO io (Result Exception (Value io) -> Step io)
    | Continue (Step io)
    | Error Exception
    | Done (Value io)


{-| Accepted by `eval`, `continueEval`, and `evalPure`. If `EvalOptions.maxOps` is not Nothing, `eval` and `continueEval`
will return a `Continue step` while `evalPure` will return an error.
-}
type alias EvalOptions =
    { maxOps : Maybe Int }


{-| Evaluates a given Enclojure program in a provided environment. Returns a tuple of
the evaluation result and modified environment. May cause an infinite loop if EvalOptions.maxOps is not specified.
-}
eval : EvalOptions -> Env io -> String -> ( EvalResult io, Env io )
eval options initEnv code =
    code
        |> Reader.readString
        |> Result.mapError (Runtime.throw initEnv)
        |> Result.map2
            (\a b -> a ++ b)
            prelude
        |> Result.andThen
            (\exprs ->
                exprs
                    |> List.head
                    |> Maybe.map ((\lv -> Located.sameAs lv exprs) >> wrapInDo)
                    |> Result.fromMaybe (Exception "Empty program" [])
            )
        |> Result.map
            (\program ->
                evalExpression program
                    initEnv
                    Runtime.terminate
            )
        |> (\r ->
                case r of
                    Ok step ->
                        continueEval options step

                    Err e ->
                        ( Error e, initEnv )
           )


toPureResult : EvalResult io -> Env io -> Result Exception ( Value io, Env io )
toPureResult evalResult retEnv =
    case evalResult of
        RunIO _ _ ->
            Value.exception "pure eval attempted a side effect"
                |> Runtime.throw retEnv
                |> Err

        Continue _ ->
            Value.exception "pure eval exceeded allotted maxOps"
                |> Runtime.throw retEnv
                |> Err

        Error ex ->
            Err ex

        Done val ->
            Ok ( val, retEnv )


{-| Evaluates a given Enclojure program in a provided environment. Returns an error on exception,
and a tuple of returned value and changed environment on successful execution. Throws if the program attempts
any side effects. May cause an infinite loop if EvalOptions.maxOps is not specified.
-}
evalPure : EvalOptions -> Env io -> String -> Result Exception ( Value io, Env io )
evalPure options initEnv code =
    let
        ( evalResult, retEnv ) =
            eval options initEnv code
    in
    toPureResult evalResult retEnv


continueEvalPure : EvalOptions -> Step io -> Result Exception ( Value io, Env io )
continueEvalPure options step =
    let
        ( evalResult, retEnv ) =
            continueEval options step
    in
    toPureResult evalResult retEnv


{-| Continue evaluation from a previously returned step.
-}
continueEval : EvalOptions -> Step io -> ( EvalResult io, Env io )
continueEval options ((Located loc ( result, thunk )) as step) =
    case result of
        Ok ( io, env ) ->
            let
                atOpsLimit =
                    options.maxOps |> Maybe.map (\maxOps -> maxOps <= 0) |> Maybe.withDefault False
            in
            if atOpsLimit then
                ( Continue step, env )

            else
                case io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                continueEval
                                    { options | maxOps = options.maxOps |> Maybe.map ((+) -1) }
                                    (continuation (Located loc v) env)

                            Nothing ->
                                ( Done v, env )

                    SideEffect se ->
                        ( RunIO se
                            (\r ->
                                ( r
                                    |> Result.map (\resultValue -> ( Const resultValue, env ))
                                    |> Result.mapError (\ex -> ( ex, env ))
                                , thunk
                                )
                                    |> Located loc
                            )
                        , env
                        )

        Err ( e, env ) ->
            ( Error e, env )


{-| Modify the execution environment of an eval step.
-}
setStepEnv : Env io -> Step io -> Step io
setStepEnv env (Located loc ( result, thunk )) =
    case result of
        Ok ( io, _ ) ->
            Located loc ( Ok ( io, env ), thunk )

        Err ( ex, _ ) ->
            Located loc ( Err ( ex, env ), thunk )


{-| Get the execution environment of an eval step.
-}
getStepEnv : Step io -> Env io
getStepEnv (Located _ ( result, _ )) =
    case result of
        Ok ( _, env ) ->
            env

        Err ( _, env ) ->
            env


{-| Get the value of an eval step unless the step returned a side effect.
-}
getStepValue : Step io -> Maybe (Value io)
getStepValue (Located _ ( result, _ )) =
    result
        |> Result.toMaybe
        |> Maybe.andThen
            (\( io, _ ) ->
                case io of
                    Const v ->
                        Just v

                    _ ->
                        Nothing
            )


type alias SpecialForm io =
    { info : FnInfo, eval : Located (List (Located (Value io))) -> Env io -> Continuation io -> Step io }


specialForms : List (SpecialForm io)
specialForms =
    [ SpecialForm
        { name = Just "def"
        , doc = Just """Creates and interns a global var with the name
of symbol or locates such a var if it already exists.  Then init is evaluated, and the
root binding of the var is set to the resulting value."""
        , signatures = [ [ "symbol", "init" ] ]
        }
        evalDef
    , SpecialForm
        { name = Just "do"
        , doc = Just """Evaluates the expressions in order and returns the value of
the last. If no expressions are supplied, returns nil."""
        , signatures = [ [ "&", "exprs" ] ]
        }
        evalDo
    , SpecialForm
        { name = Just "if"
        , doc = Just """Evaluates test. If not the singular values nil or false,
evaluates and yields then, otherwise, evaluates and yields else. If
else is not supplied it defaults to nil."""
        , signatures = [ [ "test", "then", "else?" ] ]
        }
        evalIf
    , SpecialForm
        { name = Just "quote"
        , doc = Just "Yields the unevaluated form."
        , signatures = [ [ "form" ] ]
        }
        evalQuote
    , SpecialForm
        { name = Just "let"
        , doc = Just """binding => binding-form init-expr
binding-form => name, or destructuring-form
destructuring-form => map-destructure-form, or seq-destructure-form

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein."""
        , signatures = [ [ "[bindings*]", "exprs*" ] ]
        }
        evalLet
    , SpecialForm
        { name = Just "fn"
        , doc = Just """params => positional-params*, or positional-params* & rest-param
positional-param => binding-form
rest-param => binding-form
binding-form => name, or destructuring-form

Defines a function."""
        , signatures =
            [ [ "name?", "docstring?", "[params*]", "exprs*" ]
            , [ "name?", "([params*] exprs*)", "+" ]
            ]
        }
        evalFn
    ]


specialFormsByName : Dict String (SpecialForm io)
specialFormsByName =
    List.foldl
        (\({ info } as sf) acc ->
            info.name |> Maybe.map (\n -> Dict.insert n sf acc) |> Maybe.withDefault acc
        )
        Dict.empty
        specialForms


{-| Basic function info: `name`, `doc`, and a list of `signatures`.
-}
type alias FnInfo =
    Enclojure.Common.FnInfo


{-| Represents the type of a documentation entry.
-}
type Doc
    = SpecialFormDoc
    | MacroDoc
    | FunctionDoc


{-| Returns a list of documentation entries for a given evaluation environment.
-}
documentation : Env io -> List ( Doc, FnInfo )
documentation env =
    let
        functionDocs =
            env.globalScope
                |> Dict.foldl
                    (\name v acc ->
                        case v of
                            Fn info _ ->
                                ( FunctionDoc, { info | name = Just name } ) :: acc

                            _ ->
                                acc
                    )
                    []
    in
    (functionDocs
        ++ (specialForms
                |> List.map (.info >> Tuple.pair SpecialFormDoc)
           )
        ++ (Macros.all
                |> List.map (.info >> Tuple.pair MacroDoc)
           )
    )
        |> List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
