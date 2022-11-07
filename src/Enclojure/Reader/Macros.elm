module Enclojure.Reader.Macros exposing (all, macroexpandAll)

import Array
import Dict exposing (Dict)
import Enclojure.Common exposing (Exception(..), FnInfo, Number(..), Value(..))
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Value as Value
import Enclojure.ValueMap as ValueMap


type Expansion a
    = Expanded a
    | Returned a


macroexpandAll : Located (Value io) -> Result Exception (Located (Value io))
macroexpandAll v =
    macroexpandAllInternal 0 v |> Result.map Tuple.second


macroexpandAllInternal : Int -> Located (Value io) -> Result Exception ( Int, Located (Value io) )
macroexpandAllInternal i v =
    case macroexpand i v of
        Ok result ->
            case result of
                Expanded ( nextI, val ) ->
                    macroexpandAllInternal nextI val

                Returned ( nextI, Located loc val ) ->
                    case val of
                        List l ->
                            List.foldr
                                (\e a ->
                                    a
                                        |> Result.andThen
                                            (\( ni, lr ) ->
                                                macroexpandAllInternal ni e |> Result.map (\( nni, r ) -> ( nni, r :: lr ))
                                            )
                                )
                                (Ok ( nextI, [] ))
                                l
                                |> Result.map (Tuple.mapSecond (List >> Located loc))

                        Vector l ->
                            Array.foldl
                                (\e a -> a |> Result.andThen (\( ni, lr ) -> macroexpandAllInternal ni e |> Result.map (\( nni, r ) -> ( nni, Array.push r lr ))))
                                (Ok ( nextI, Array.empty ))
                                l
                                |> Result.map (Tuple.mapSecond (Vector >> Located loc))

                        Map m ->
                            m
                                |> ValueMap.toList
                                |> List.foldl
                                    (\( mapKey, mapVal ) a -> a |> Result.andThen (\( ni, lr ) -> macroexpandAllInternal ni mapVal |> Result.map (\( nni, r ) -> ( nni, ( mapKey, r ) :: lr ))))
                                    (Ok ( nextI, [] ))
                                |> Result.map (Tuple.mapSecond (ValueMap.fromList >> Map >> Located loc))

                        _ ->
                            Ok ( nextI, Located loc val )

        Err e ->
            Err e


type alias Macro io =
    { info : FnInfo
    , expand : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
    }


all : List (Macro io)
all =
    [ Macro
        { name = Just "__lambda"
        , doc = Just "Inserted by the reader in place of #()."
        , signatures = []
        }
        expandLambda
    , Macro
        { name = Just "and"
        , doc = Just """Evaluates exprs one at a time, from left to right. If a form
returns logical false (nil or false), and returns that value and
doesn't evaluate any of the other expressions, otherwise it returns
the value of the last expr. (and) returns true."""
        , signatures = [ [ "x" ], [ "x", "&", "next" ] ]
        }
        expandAnd
    , Macro
        { name = Just "case"
        , doc = Just """Takes an expression and a set of test/expr pairs. Each clause can take the form of either:
  test-constant result-expr
  (test-constant1 ... test-constantN)  result-expr

  The test-constants are not evaluated. They must be compile-time
  literals, and need not be quoted.  If the expression is equal to a
  test-constant, the corresponding result-expr is returned. A single
  default expression can follow the clauses, and its value will be
  returned if no clause matches. If no default expression is provided
  and no clause matches, an exception is thrown.

  Unlike Clojure, the clauses are considered sequentially.
  The current implementation doesn't throw on redundant test expressions.
  All manner of constant
  expressions are acceptable in case, including numbers, strings,
  symbols, keywords, and (Clojure) composites thereof. Note that since
  lists are used to group multiple constants that map to the same
  expression, a vector can be used to match a list if needed. The
  test-constants need not be all of the same type.
"""
        , signatures = [ [ "e", "&", "clauses" ] ]
        }
        expandCase
    , Macro
        { name = Just "cond"
        , doc = Just """Takes a set of test/expr pairs. It evaluates each test one at a
time.  If a test returns logical true, cond evaluates and returns
the value of the corresponding expr and doesn't evaluate any of the
other tests or exprs. (cond) returns nil.

If the test is a :let keyword, the next test/expr pair will be wrapped in a let expression with the values supplied
as the expr for the :let.
"""
        , signatures = [ [ "&", "clauses" ] ]
        }
        expandCond
    , Macro
        { name = Just "defn"
        , doc = Just """Same as (def name "doc" (fn [params* ] exprs*)) or (def
name (fn "doc" ([params* ] exprs*)+))."""
        , signatures = [ [ "name", "doc-string?", "[params*]", "body" ], [ "name", "doc-string?", "&", "bodies" ] ]
        }
        expandDefn
    , Macro
        { name = Just "doseq"
        , doc = Just """Repeatedly executes body (presumably for side-effects) with
bindings and filtering as provided by "for".  Does not retain
the head of the sequence. Returns nil."""
        , signatures = [ [ "seq-exprs", "&", "body" ] ]
        }
        expandDoseq
    , Macro
        { name = Just "dotimes"
        , doc = Just """bindings => name n

Repeatedly executes body (presumably for side-effects) with name
bound to integers from 0 through n-1."""
        , signatures = [ [ "bindings", "& body" ] ]
        }
        expandDotimes
    , Macro
        { name = Just "for"
        , doc = Just """List comprehension. Takes a vector of one or more
 binding-form/collection-expr pairs, each followed by zero or more
 modifiers, and yields a list of evaluations of expr.
 Collections are iterated in a nested fashion, rightmost fastest,
 and nested coll-exprs can refer to bindings created in prior
 binding-forms.  Supported modifiers are: :let [binding-form expr ...],
 :when test.

(take 100 (for [x (range 100000000) y (range 1000000) :when (< y x)] [x y]))"""
        , signatures = [ [ "seq-exprs", "body-expr" ] ]
        }
        expandFor
    , Macro
        { name = Just "if-let"
        , doc = Just """bindings => binding-form test

If test is true, evaluates then with binding-form bound to the value of
test, if not, yields else"""
        , signatures = [ [ "bindings", "then" ], [ "bindings", "then", "else", "&", "oldform" ] ]
        }
        expandIfLet
    , Macro
        { name = Just "loop"
        , doc = Just """Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. Acts as a recur target."""
        , signatures = [ [ "[bindings*]", "exprs*" ] ]
        }
        expandLoop
    , Macro
        { name = Just "or"
        , doc = Just """Evaluates exprs one at a time, from left to right. If a form
returns a logical true value, or returns that value and doesn't
evaluate any of the other expressions, otherwise it returns the
value of the last expression. (or) returns nil."""
        , signatures = [ [], [ "x" ], [ "x", "&", "next" ] ]
        }
        expandOr
    , Macro
        { name = Just "some->"
        , doc = Just """When expr is not nil, threads it into the first form (via ->),
and when that result is not nil, through the next etc"""
        , signatures = [ [ "expr", "&", "forms" ] ]
        }
        expandThreadSomeFirst
    , Macro
        { name = Just "some->>"
        , doc = Just """When expr is not nil, threads it into the first form (via ->>),
and when that result is not nil, through the next etc"""
        , signatures = [ [ "expr", "&", "forms" ] ]
        }
        expandThreadSomeLast
    , Macro
        { name = Just "when"
        , doc = Just """Evaluates test. If logical true, evaluates body in an implicit do."""
        , signatures = [ [ "test", "&", "body" ] ]
        }
        expandWhen
    , Macro
        { name = Just "when-let"
        , doc = Just """bindings => binding-form test

When test is true, evaluates body with binding-form bound to the value of test"""
        , signatures = [ [ "bindings", "&", "body" ] ]
        }
        expandWhenLet
    , Macro
        { name = Just "when-not"
        , doc = Just "Evaluates test. If logical false, evaluates body in an implicit do."
        , signatures = [ [ "test", "&", "body" ] ]
        }
        expandWhenNot
    , Macro
        { name = Just "while"
        , doc = Just """Repeatedly executes body while test expression is true. Presumes
some side-effect will cause test to become false/nil. Returns nil"""
        , signatures = [ [ "test", "&", "body" ] ]
        }
        expandWhile
    , Macro
        { name = Just "->"
        , doc = Just """Threads the expr through the forms. Inserts x as the
second item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
second item in second form, etc."""
        , signatures = [ [ "x", "&", "forms" ] ]
        }
        expandThreadFirst
    , Macro
        { name = Just "->>"
        , doc = Just """Threads the expr through the forms. Inserts x as the
last item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
last item in second form, etc."""
        , signatures = [ [ "x", "&", "forms" ] ]
        }
        expandThreadLast
    ]


allByName : Dict String (Macro io)
allByName =
    List.foldl
        (\({ info } as macro) acc ->
            info.name |> Maybe.map (\n -> Dict.insert n macro acc) |> Maybe.withDefault acc
        )
        Dict.empty
        all


macroexpand : Int -> Located (Value io) -> Result Exception (Expansion ( Int, Located (Value io) ))
macroexpand i (Located loc value) =
    case value of
        List l ->
            case l of
                (Located _ (Symbol name)) :: args ->
                    allByName
                        |> Dict.get name
                        |> Maybe.map (\{ expand } -> expand i (Located loc args))
                        |> Maybe.map (Result.map Expanded)
                        |> Maybe.withDefault (Ok (Returned ( i, Located loc value )))

                _ ->
                    Ok (Returned ( i, Located loc value ))

        _ ->
            Ok (Returned ( i, Located loc value ))


expandDefn : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandDefn i (Located loc args) =
    case args of
        (Located _ (Symbol name)) :: (Located _ (String doc)) :: fnBody ->
            Ok
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "def")
                        , Located loc (Symbol name)
                        , Located loc
                            (List
                                (Located loc (Symbol "fn")
                                    :: Located loc (Symbol name)
                                    :: Located loc (String doc)
                                    :: fnBody
                                )
                            )
                        ]
                    )
                )

        (Located _ (Symbol name)) :: fnBody ->
            Ok
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "def")
                        , Located loc (Symbol name)
                        , Located loc
                            (List
                                (Located loc (Symbol "fn")
                                    :: Located loc (Symbol name)
                                    :: fnBody
                                )
                            )
                        ]
                    )
                )

        _ ->
            Err (Exception "Argument error: invalid arguments to defn" [])


expandDotimes : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandDotimes i (Located loc args) =
    case args of
        (Located _ (Vector bindings)) :: body ->
            case Array.toList bindings of
                (Located _ ((Symbol _) as binding)) :: (Located _ ((Number (Int _)) as nTimes)) :: [] ->
                    Ok
                        ( i
                        , (Located loc (Value.symbol "doseq")
                            :: Located loc (Value.vectorFromList [ binding, Value.list [ Value.symbol "range", nTimes ] ])
                            :: body
                          )
                            |> List
                            |> Located loc
                        )

                _ ->
                    Err (Value.exception "Argument error: invalid arguments to dotimes")

        _ ->
            Err (Value.exception "Argument error: invalid arguments to dotimes")


expandDoseq : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandDoseq i (Located loc args) =
    case args of
        (Located bindingsPos (Vector bindings)) :: body ->
            case Array.toList bindings of
                (Located letPos (Keyword "let")) :: ((Located _ (Vector _)) as letBindings) :: rest ->
                    expandDoseq
                        i
                        (Located loc (Located bindingsPos (Value.vectorFromLocatedList rest) :: body))
                        |> Result.map
                            (\( retI, ret ) ->
                                ( retI
                                , Located loc
                                    (List
                                        [ Located letPos (Symbol "let")
                                        , letBindings
                                        , ret
                                        ]
                                    )
                                )
                            )

                (Located whenPos (Keyword "when")) :: whenCond :: rest ->
                    expandDoseq
                        i
                        (Located loc (Located bindingsPos (Value.vectorFromLocatedList rest) :: body))
                        |> Result.map
                            (\( retI, ret ) ->
                                ( retI
                                , Located whenPos
                                    (List
                                        [ Located whenPos (Symbol "do")
                                        , Located whenPos
                                            (List
                                                [ Located whenPos (Symbol "when")
                                                , whenCond
                                                , ret
                                                ]
                                            )
                                        , Located whenPos Nil
                                        ]
                                    )
                                )
                            )

                ((Located mapLoc _) as mapArg) :: mappedSeq :: rest ->
                    expandDoseq
                        i
                        (Located loc (Located bindingsPos (Value.vectorFromLocatedList rest) :: body))
                        |> Result.map
                            (\( retI, ret ) ->
                                ( retI
                                , Located mapLoc
                                    (List
                                        [ Located mapLoc (Symbol "do")
                                        , Located mapLoc
                                            (List
                                                [ Located mapLoc (Symbol "map")
                                                , Located mapLoc
                                                    (List
                                                        [ Located mapLoc (Symbol "fn")
                                                        , Located mapLoc (Value.vectorFromLocatedList [ mapArg ])
                                                        , ret
                                                        ]
                                                    )
                                                , mappedSeq
                                                ]
                                            )
                                        , Located mapLoc Nil
                                        ]
                                    )
                                )
                            )

                [ _ ] ->
                    Err (Value.exception "Argument error: uneven number of bindings to doseq")

                [] ->
                    Ok ( i, Located loc (List (Located loc (Symbol "do") :: body)) )

        _ ->
            Err (Value.exception "Argument error: invalid arguments to doseq")


expandFor : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandFor i (Located loc args) =
    let
        id =
            "for__" ++ String.fromInt i ++ "__auto__"
    in
    case args of
        ((Located _ (Vector _)) as bindings) :: (Located _ body) :: [] ->
            Ok
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc
                            (Value.vectorFromList
                                [ Value.symbol id
                                , Value.list [ Value.symbol "atom", Value.vectorFromList [] ]
                                ]
                            )
                        , Located loc
                            (List
                                [ Located loc (Symbol "doseq")
                                , bindings
                                , Located loc
                                    (Value.list
                                        [ Value.symbol "swap!"
                                        , Value.symbol id
                                        , Value.symbol "conj"
                                        , body
                                        ]
                                    )
                                ]
                            )
                        , Located loc (Value.list [ Symbol "deref", Value.symbol id ])
                        ]
                    )
                )

        _ ->
            Err (Value.exception "for expects a vector of bindings followed by the body")


expandIfLet : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandIfLet i (Located loc args) =
    case args of
        (Located _ (Vector bindings)) :: branches ->
            case Array.toList bindings of
                n :: v :: [] ->
                    Ok
                        ( i
                        , Located loc
                            (List
                                [ Located loc (Symbol "let")
                                , Located loc (Vector (Array.fromList [ n, v ]))
                                , Located loc (List (Located loc (Symbol "if") :: n :: branches))
                                ]
                            )
                        )

                _ ->
                    Err (Exception "Argument error: more than 2 elements in bindings array to if-let" [])

        _ ->
            Err (Exception "Argument error: invalid arguments to if-let" [])


expandWhenLet : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandWhenLet i (Located loc args) =
    case args of
        (Located _ (Vector bindings)) :: do ->
            case Array.toList bindings of
                n :: v :: [] ->
                    Ok
                        ( i
                        , Located loc
                            (List
                                [ Located loc (Symbol "let")
                                , Located loc (Vector (Array.fromList [ n, v ]))
                                , Located loc (List (Located loc (Symbol "when") :: n :: do))
                                ]
                            )
                        )

                _ ->
                    Err (Exception "Argument error: more than 2 elements in bindings array to if-let" [])

        _ ->
            Err (Exception "Argument error: invalid arguments to when-let" [])


expandAnd : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandAnd i (Located loc args) =
    case args of
        (Located _ form) :: rest ->
            let
                id =
                    "and__" ++ String.fromInt i ++ "__auto__"
            in
            Ok <|
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc (Vector (Array.fromList [ Located loc (Symbol id), Located loc form ]))
                        , Located loc
                            (List
                                [ Located loc (Symbol "if")
                                , Located loc (Symbol id)
                                , if List.isEmpty rest then
                                    Located loc (Symbol id)

                                  else
                                    Located loc (List (Located loc (Symbol "and") :: rest))
                                , Located loc (Symbol id)
                                ]
                            )
                        ]
                    )
                )

        [] ->
            Ok ( i, Located loc (Bool True) )


expandLoop : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandLoop i (Located loc args) =
    let
        toArgsAndValues argSymbols values bindings =
            case bindings of
                [] ->
                    Ok ( List.reverse argSymbols, List.reverse values )

                a :: v :: rest ->
                    toArgsAndValues (a :: argSymbols) (v :: values) rest

                _ ->
                    Err (Value.exception "Uneven number of loop bindings")
    in
    case args of
        (Located _ (Vector bindings)) :: loopBody ->
            bindings
                |> Array.toList
                |> toArgsAndValues [] []
                |> Result.map
                    (\( argSymbols, argValues ) ->
                        ( i
                        , Located loc
                            (List
                                (Located loc
                                    (List
                                        (Located loc (Symbol "fn")
                                            :: Located loc (Vector (Array.fromList argSymbols))
                                            :: loopBody
                                        )
                                    )
                                    :: argValues
                                )
                            )
                        )
                    )

        _ ->
            Err <| Value.exception "Invalid number of arguments to loop"


expandOr : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandOr i (Located loc args) =
    case args of
        (Located _ form) :: rest ->
            let
                id =
                    "or__" ++ String.fromInt i ++ "__auto__"
            in
            Ok <|
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc (Vector (Array.fromList [ Located loc (Symbol id), Located loc form ]))
                        , Located loc
                            (List
                                [ Located loc (Symbol "if")
                                , Located loc (Symbol id)
                                , Located loc (Symbol id)
                                , Located loc (List (Located loc (Symbol "or") :: rest))
                                ]
                            )
                        ]
                    )
                )

        [] ->
            Ok ( i, Located loc Nil )


expandWhen : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandWhen i (Located loc args) =
    case args of
        cond :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "if")
                        , cond
                        , Located loc
                            (List
                                (Located loc (Symbol "do") :: rest)
                            )
                        ]
                    )
                )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to when" [])


expandWhenNot : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandWhenNot i (Located loc args) =
    case args of
        cond :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "if")
                        , Located loc (List [ Located loc (Symbol "not"), cond ])
                        , Located loc
                            (List
                                (Located loc (Symbol "do") :: rest)
                            )
                        ]
                    )
                )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to when-not" [])


expandWhile : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandWhile i (Located loc args) =
    case args of
        test :: body ->
            Ok
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "loop")
                        , Located loc (Vector Array.empty)
                        , Located loc
                            (List
                                (Located loc (Symbol "when")
                                    :: test
                                    :: Located loc (List (Located loc (Symbol "do") :: body))
                                    :: [ Located loc (List [ Located loc (Symbol "recur") ]) ]
                                )
                            )
                        ]
                    )
                )

        _ ->
            Err (Value.exception "invalid number of arguments provided to while")


expandCase : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandCase i (Located loc args) =
    case args of
        (Located exprLoc expr) :: clauses ->
            let
                id =
                    "case__" ++ String.fromInt i ++ "__auto__"

                parseClauses remainingClauses =
                    case remainingClauses of
                        (Located testLoc (List tests)) :: ret :: rest ->
                            Located testLoc
                                (List
                                    [ Located testLoc (Symbol "or")
                                    , parseClauses (List.concatMap (\test -> [ test, ret ]) tests)
                                    , parseClauses rest
                                    ]
                                )

                        (Located testLoc test) :: ret :: rest ->
                            Located testLoc
                                (List
                                    [ Located testLoc (Symbol "if")
                                    , Located testLoc
                                        (List
                                            [ Located testLoc (Symbol "=")
                                            , Located testLoc (Symbol id)
                                            , Located testLoc
                                                (List
                                                    [ Located testLoc (Symbol "quote")
                                                    , Located testLoc test
                                                    ]
                                                )
                                            ]
                                        )
                                    , ret
                                    , parseClauses rest
                                    ]
                                )

                        [ defaultClause ] ->
                            defaultClause

                        [] ->
                            Located loc
                                (List
                                    [ Located loc (Symbol "throw")
                                    , Located loc
                                        (List
                                            [ Located loc (Symbol "Exception.")
                                            , Located loc
                                                (List
                                                    [ Located loc (Symbol "str")
                                                    , Located loc (String "No matching clause: ")
                                                    , Located exprLoc (Symbol id)
                                                    ]
                                                )
                                            ]
                                        )
                                    ]
                                )
            in
            Ok
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc
                            (Vector (Array.fromList [ Located loc (Symbol id), Located loc expr ]))
                        , parseClauses clauses
                        ]
                    )
                )

        _ ->
            Err <| Value.exception "Wrong number of args (0) passed to: case"


expandCond : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandCond i (Located loc args) =
    case args of
        (Located letLoc (Keyword "let")) :: bindings :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located letLoc (Symbol "let")
                        , bindings
                        , Located loc (List (Located loc (Symbol "cond") :: rest))
                        ]
                    )
                )

        condForm :: thenForm :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "if")
                        , condForm
                        , thenForm
                        , Located loc (List (Located loc (Symbol "cond") :: rest))
                        ]
                    )
                )

        [ _ ] ->
            Err (Exception "compilation error: cond has uneven number of forms" [])

        [] ->
            Ok <| ( i, Located loc Nil )


expandThreadFirst : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandThreadFirst i (Located loc args) =
    case args of
        arg :: op :: rest ->
            Ok
                ( i
                , Located loc
                    (List
                        (Located loc (Symbol "->")
                            :: (case op of
                                    Located _ (List forms) ->
                                        (\fn restArgs ->
                                            Located.sameAs fn (List (fn :: arg :: restArgs))
                                        )
                                            (List.head forms |> Maybe.withDefault (Located loc Nil))
                                            (List.tail forms |> Maybe.withDefault [])

                                    _ ->
                                        Located.sameAs arg (List [ op, arg ])
                               )
                            :: rest
                        )
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to ->" [])


expandThreadLast : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandThreadLast i (Located loc args) =
    case args of
        arg :: op :: rest ->
            Ok
                ( i
                , Located loc
                    (List
                        (Located loc (Symbol "->>")
                            :: (case op of
                                    Located lloc (List forms) ->
                                        Located lloc (List (forms ++ [ arg ]))

                                    _ ->
                                        Located.sameAs arg (List [ op, arg ])
                               )
                            :: rest
                        )
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to ->>" [])


expandThreadSomeFirst : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandThreadSomeFirst i (Located loc args) =
    case args of
        someArg :: op :: rest ->
            let
                binding =
                    Located loc (Symbol ("some->__" ++ String.fromInt i ++ "__auto__"))
            in
            Ok
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "when-let")
                        , Located loc (Vector (Array.fromList [ binding, someArg ]))
                        , Located loc
                            (List
                                (Located loc (Symbol "some->")
                                    :: (case op of
                                            Located _ (List forms) ->
                                                (\fn restArgs ->
                                                    Located.sameAs fn (List (fn :: binding :: restArgs))
                                                )
                                                    (List.head forms |> Maybe.withDefault (Located loc Nil))
                                                    (List.tail forms |> Maybe.withDefault [])

                                            _ ->
                                                Located.sameAs binding (List [ op, binding ])
                                       )
                                    :: rest
                                )
                            )
                        ]
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to some->" [])


expandThreadSomeLast : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandThreadSomeLast i (Located loc args) =
    case args of
        someArg :: op :: rest ->
            let
                binding =
                    Located loc (Symbol ("some->>__" ++ String.fromInt i ++ "__auto__"))
            in
            Ok
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "when-let")
                        , Located loc (Vector (Array.fromList [ binding, someArg ]))
                        , Located loc
                            (List
                                (Located loc (Symbol "some->>")
                                    :: (case op of
                                            Located lloc (List forms) ->
                                                Located lloc (List (forms ++ [ binding ]))

                                            _ ->
                                                Located.sameAs binding (List [ op, binding ])
                                       )
                                    :: rest
                                )
                            )
                        ]
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to some->>" [])


walk : a -> (a -> Located (Value io) -> Result (Located Exception) ( a, Located (Value io) )) -> Located (Value io) -> Result (Located Exception) ( a, Located (Value io) )
walk state f (Located loc val) =
    case val of
        List l ->
            l
                |> List.foldr (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState f e |> Result.map (\( nState, v ) -> ( nState, v :: av )))) (Ok ( state, [] ))
                |> Result.map (\( s, r ) -> ( s, Located loc (List r) ))

        Vector l ->
            l
                |> Array.foldl (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState f e |> Result.map (\( nState, v ) -> ( nState, Array.push v av )))) (Ok ( state, Array.empty ))
                |> Result.map (\( s, r ) -> ( s, Located loc (Vector r) ))

        Map m ->
            m
                |> ValueMap.toList
                |> List.foldr
                    (\( mapKey, mapVal ) a ->
                        a
                            |> Result.andThen
                                (\( aState, av ) ->
                                    walk aState f mapVal |> Result.map (\( nState, v ) -> ( nState, ( mapKey, v ) :: av ))
                                )
                    )
                    (Ok ( state, [] ))
                |> Result.map (\( s, r ) -> ( s, Located loc (Map (ValueMap.fromList r)) ))

        _ ->
            f state (Located loc val)


type alias Arguments =
    { positional : Dict Int String
    , variadic : Maybe String
    }


expandLambda : Int -> Located (List (Located (Value io))) -> Result Exception ( Int, Located (Value io) )
expandLambda i (Located loc body) =
    case body of
        [] ->
            Ok ( i, Located loc (List [ Located loc (Symbol "fn"), Located loc (Vector Array.empty), Located loc (List []) ]) )

        exprs ->
            case substituteLambdaArgs i exprs of
                Ok ( ( newI, args ), newExprs ) ->
                    let
                        ( finalI, completedArgs ) =
                            completeArguments newI args
                    in
                    Ok ( finalI, Located loc (List [ Located loc (Symbol "fn"), Located loc (argsToValue (Located loc completedArgs)), Located loc (List newExprs) ]) )

                Err e ->
                    Err (Located.getValue e)


completeArguments : Int -> Arguments -> ( Int, Arguments )
completeArguments startI arguments =
    let
        maxPositional =
            List.maximum (Dict.keys arguments.positional) |> Maybe.withDefault 0
    in
    List.range 1 maxPositional
        |> List.foldr
            (\e ( i, a ) ->
                let
                    ( newI, id ) =
                        a.positional
                            |> Dict.get e
                            |> Maybe.map (Tuple.pair i)
                            |> Maybe.withDefault ( i + 1, "p" ++ String.fromInt e ++ "__" ++ String.fromInt i )
                in
                ( newI, { a | positional = Dict.insert e id a.positional } )
            )
            ( startI, arguments )


argsToValue : Located Arguments -> Value io
argsToValue (Located loc arguments) =
    let
        positional =
            arguments.positional
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (Tuple.second >> Symbol >> Located loc)

        variadic =
            arguments.variadic |> Maybe.map (Symbol >> Located loc >> List.singleton >> (++) [ Located loc (Symbol "&") ]) |> Maybe.withDefault []
    in
    Vector (Array.fromList (positional ++ variadic))


substituteLambdaArgsWalker : ( Int, Arguments ) -> Located (Value io) -> Result (Located Exception) ( ( Int, Arguments ), Located (Value io) )
substituteLambdaArgsWalker ( i, args ) (Located loc expr) =
    case expr of
        Symbol "__lambda" ->
            Err (Located loc (Exception "Parsing error: nested #() are not supported, use fn instead." []))

        Symbol "%&" ->
            let
                ( newI, id ) =
                    args.variadic |> Maybe.map (Tuple.pair i) |> Maybe.withDefault ( i + 1, "rest__" ++ String.fromInt i )
            in
            Ok ( ( newI, { args | variadic = Just id } ), Located loc (Symbol id) )

        Symbol name ->
            let
                argN =
                    if name == "%" then
                        Just 1

                    else if String.startsWith "%" name then
                        String.toInt (String.dropLeft 1 name)
                            |> Maybe.andThen
                                (\n ->
                                    if n < 1 then
                                        Nothing

                                    else
                                        Just n
                                )

                    else
                        Nothing
            in
            case argN of
                Just n ->
                    let
                        ( newI, id ) =
                            Dict.get n args.positional |> Maybe.map (Tuple.pair i) |> Maybe.withDefault ( i + 1, "p" ++ String.fromInt n ++ "__" ++ String.fromInt i )
                    in
                    Ok ( ( newI, { args | positional = Dict.insert n id args.positional } ), Located loc (Symbol id) )

                Nothing ->
                    Ok ( ( i, args ), Located loc expr )

        _ ->
            Ok ( ( i, args ), Located loc expr )


substituteLambdaArgs : Int -> List (Located (Value io)) -> Result (Located Exception) ( ( Int, Arguments ), List (Located (Value io)) )
substituteLambdaArgs i exprs =
    let
        startState =
            ( i, { positional = Dict.empty, variadic = Nothing } )
    in
    exprs
        |> List.foldr
            (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState substituteLambdaArgsWalker e |> Result.map (\( nState, v ) -> ( nState, v :: av ))))
            (Ok ( startState, [] ))
