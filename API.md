# Enclojure API

## *

`(function)`

Usage:

```
(*)
(* x)
(* x y & more)
```

Returns the product of nums. (*) returns 1.

## +

`(function)`

Usage:

```
(+)
(+ x)
(+ x y & more)
```

Returns the sum of nums. (+) returns 0.

## -

`(function)`

Usage:

```
(- x)
(- x y & more)
```

If no ys are supplied, returns the negation of x, else subtracts the ys from x and returns the result.

## ->

`(macro)`

Usage:

```
(-> x & forms)
```

Threads the expr through the forms. Inserts x as the
second item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
second item in second form, etc.

## ->>

`(macro)`

Usage:

```
(->> x & forms)
```

Threads the expr through the forms. Inserts x as the
last item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
last item in second form, etc.

## /

`(function)`

Usage:

```
(/ x)
(/ x y & more)
```

If no denominators are supplied, returns 1/numerator, else returns numerator divided by all of the denominators.

## <

`(function)`

Usage:

```
(< x)
(< x y & more)
```

Returns non-nil if nums or strings are in monotonically increasing order, otherwise false.

## <=

`(function)`

Usage:

```
(<= x)
(<= x y & more)
```

Returns non-nil if nums or strings are in monotonically non-decreasing order, otherwise false.

## >

`(function)`

Usage:

```
(> x)
(> x y & more)
```

Returns non-nil if nums are in monotonically decreasing order, otherwise false.

## >=

`(function)`

Usage:

```
(>= x)
(>= x y & more)
```

Returns non-nil if nums are in monotonically non-increasing order, otherwise false.

## __lambda

`(macro)`

Usage:

```

```

Inserted by the reader in place of #().

## abs

`(function)`

Usage:

```
(abs x)
```

Returns the absolute value of a.
   If a is a double and zero => +0.0
   If a is a double and ##Inf or ##-Inf => ##Inf
   If a is a double and ##NaN => ##NaN

## and

`(macro)`

Usage:

```
(and x)
(and x & next)
```

Evaluates exprs one at a time, from left to right. If a form
returns logical false (nil or false), and returns that value and
doesn't evaluate any of the other expressions, otherwise it returns
the value of the last expr. (and) returns true.

## atom

`(function)`

Usage:

```
(atom x)
```

Creates and returns an Atom with an initial value of x.

## case

`(macro)`

Usage:

```
(case e & clauses)
```

Takes an expression and a set of test/expr pairs. Each clause can take the form of either:
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


## comp

`(function)`

Usage:

```
(comp & fns)
```

Takes a set of functions and returns a fn that is the composition
   of those fns.  The returned fn takes a variable number of args,
   applies the rightmost of fns to the args, the next
   fn (right-to-left) to the result, etc.

## complement

`(function)`

Usage:

```
(complement f)
```

Takes a fn f and returns a fn that takes the same arguments as f, has the same effects,
   if any, and returns the opposite truth value.

## concat

`(function)`

Usage:

```
(concat & colls)
```

Returns a seq representing the concatenation of the elements in the supplied colls.

## cond

`(macro)`

Usage:

```
(cond & clauses)
```

Takes a set of test/expr pairs. It evaluates each test one at a
time.  If a test returns logical true, cond evaluates and returns
the value of the corresponding expr and doesn't evaluate any of the
other tests or exprs. (cond) returns nil.

If the test is a :let keyword, the next test/expr pair will be wrapped in a let expression with the values supplied
as the expr for the :let.


## conj

`(function)`

Usage:

```
(conj coll x & xs)
```

conj[oin]. Returns a new collection with the xs
'added'. (conj nil item) returns (item).
(conj coll) returns coll. (conj) returns [].
The 'addition' may happen at different 'places' depending
on the concrete type.

## cons

`(function)`

Usage:

```
(cons x seq)
```

Returns a new seq where x is the first element and seq is the rest.

## constantly

`(function)`

Usage:

```
(constantly x)
```

Returns a function that takes any number of arguments and returns x.

## contains?

`(function)`

Usage:

```
(contains? coll key)
```

Returns true if key is present in the given collection, otherwise
returns false.  Note that for numerically indexed collections like
vectors, this tests if the numeric key is within the
range of indexes. 'contains?' operates constant or logarithmic time;
it will not perform a linear search for a value. See also 'some'.

## count

`(function)`

Usage:

```
(count coll)
```

Returns the number of items in coll. (count nil) returns 0. Also works on strings.

## dec

`(function)`

Usage:

```
(dec x)
```

Returns a number one less than num.

## dedupe

`(function)`

Usage:

```
(dedupe coll)
```

Returns a list removing consecutive duplicates in coll.

## def

`(special form)`

Usage:

```
(def symbol init)
```

Creates and interns a global var with the name
of symbol or locates such a var if it already exists.  Then init is evaluated, and the
root binding of the var is set to the resulting value.

## defn

`(macro)`

Usage:

```
(defn name doc-string? [params*] body)
(defn name doc-string? & bodies)
```

Same as (def name "doc" (fn [params* ] exprs*)) or (def
name (fn "doc" ([params* ] exprs*)+)).

## deref

`(function)`

Usage:

```
(deref ref)
```

Also reader macro: @var/@atom. When applied to a var or atom, returns its current state.

## distinct

`(function)`

Usage:

```
(distinct coll)
```

Returns a list of the elements of coll with duplicates removed.

## distinct?

`(function)`

Usage:

```
(distinct? x & args)
```

Returns true if no two of the arguments are =

## do

`(special form)`

Usage:

```
(do & exprs)
```

Evaluates the expressions in order and returns the value of
the last. If no expressions are supplied, returns nil.

## doseq

`(macro)`

Usage:

```
(doseq seq-exprs & body)
```

Repeatedly executes body (presumably for side-effects) with
bindings and filtering as provided by "for".  Does not retain
the head of the sequence. Returns nil.

## dotimes

`(macro)`

Usage:

```
(dotimes bindings & body)
```

bindings => name n

Repeatedly executes body (presumably for side-effects) with name
bound to integers from 0 through n-1.

## drop

`(function)`

Usage:

```
(drop n coll)
```

Returns a list of all but the first n items in coll.

## drop-while

`(function)`

Usage:

```
(drop-while pred coll)
```

Returns a list of the items in coll starting from the first item for which (pred item) returns logical false.

## empty

`(function)`

Usage:

```
(empty coll)
```

Returns an empty collection of the same type as coll, or nil.

## empty?

`(function)`

Usage:

```
(empty? coll)
```

Returns true if coll has no items - same as (not (seq coll)).
   Please use the idiom (seq x) rather than (not (empty? x))

## even?

`(function)`

Usage:

```
(even? n)
```

Returns true if n is even, throws an exception if n is not an integer.

## every?

`(function)`

Usage:

```
(every? pred coll)
```

Returns true if (pred x) is logical true for every x in coll, else false.

## false?

`(function)`

Usage:

```
(false? x)
```

Returns true if x is false, false otherwise.

## filter

`(function)`

Usage:

```
(filter pred coll)
```

Returns a list of the items in coll for which (pred item) returns logical true.

## fn

`(special form)`

Usage:

```
(fn name? docstring? [params*] exprs*)
(fn name? ([params*] exprs*) +)
```

params => positional-params*, or positional-params* & rest-param
positional-param => binding-form
rest-param => binding-form
binding-form => name, or destructuring-form

Defines a function.

## fnil

`(function)`

Usage:

```
(fnil f default)
```

Takes a function f, and returns a function that calls f, replacing
   a nil first argument to f with the supplied value x. Higher arity
   versions can replace arguments in the second and third
   positions (y, z). Note that the function f can take any number of
   arguments, not just the one(s) being nil-patched.

## for

`(macro)`

Usage:

```
(for seq-exprs body-expr)
```

List comprehension. Takes a vector of one or more
 binding-form/collection-expr pairs, each followed by zero or more
 modifiers, and yields a list of evaluations of expr.
 Collections are iterated in a nested fashion, rightmost fastest,
 and nested coll-exprs can refer to bindings created in prior
 binding-forms.  Supported modifiers are: :let [binding-form expr ...],
 :when test.

(take 100 (for [x (range 100000000) y (range 1000000) :when (< y x)] [x y]))

## identity

`(function)`

Usage:

```
(identity a)
```

Returns its argument.

## if

`(special form)`

Usage:

```
(if test then else?)
```

Evaluates test. If not the singular values nil or false,
evaluates and yields then, otherwise, evaluates and yields else. If
else is not supplied it defaults to nil.

## if-let

`(macro)`

Usage:

```
(if-let bindings then)
(if-let bindings then else & oldform)
```

bindings => binding-form test

If test is true, evaluates then with binding-form bound to the value of
test, if not, yields else

## inc

`(function)`

Usage:

```
(inc x)
```

Returns a number one greater than num.

## into

`(function)`

Usage:

```
(into to from)
```

Returns a new coll consisting of to-coll with all of the items of from-coll conjoined.

## json/decode

`(function)`

Usage:

```
(json/decode s)
```

Attempt to decode a JSON string s as an Enclojure value.

## json/encode

`(function)`

Usage:

```
(json/encode x)
```

Encode x as a JSON string.

## keep

`(function)`

Usage:

```
(keep f coll)
```

Returns a list of the non-nil results of (f item). Note, this means false return values will be included.

## keep-indexed

`(function)`

Usage:

```
(keep-indexed f coll)
```

Returns a list of the non-nil results of (f index item). Note, this means false return values will be included.

## keys

`(function)`

Usage:

```
(keys map)
```

Returns a list of the map's keys, in the same order as (seq map).

## keyword?

`(function)`

Usage:

```
(keyword? x)
```

Return true if x is a Keyword

## last

`(function)`

Usage:

```
(last coll)
```

Return the last item in coll, in linear time.

## let

`(special form)`

Usage:

```
(let [bindings*] exprs*)
```

binding => binding-form init-expr
binding-form => name, or destructuring-form
destructuring-form => map-destructure-form, or seq-destructure-form

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein.

## list

`(function)`

Usage:

```
(list)
```

Creates a new list containing the items.

## list?

`(function)`

Usage:

```
(list? x)
```

Return true if x is a List

## loop

`(macro)`

Usage:

```
(loop [bindings*] exprs*)
```

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. Acts as a recur target.

## map

`(function)`

Usage:

```
(map f coll)
```

Returns a list consisting of the result of applying f to
   first item of coll, followed by applying f to the
   second item in coll, until coll
   exhausted.

## map-entry?

`(function)`

Usage:

```
(map-entry? x)
```

Return true if x is a MapEntry

## map-indexed

`(function)`

Usage:

```
(map-indexed f coll)
```

Returns a list consisting of the result of applying f to 0
   and the first item of coll, followed by applying f to 1 and the second
   item in coll, etc, until coll is exhausted. Thus function f should
   accept 2 arguments, index and item.

## map?

`(function)`

Usage:

```
(map? x)
```

Return true if x is a Map

## mapcat

`(function)`

Usage:

```
(mapcat f coll)
```

Returns the result of applying concat to the result of applying map
   to f and coll. Thus function f should return a collection.

## max

`(function)`

Usage:

```
(max x & rst)
```

Returns the greatest of the nums.

## min

`(function)`

Usage:

```
(min x & rst)
```

Returns the least of the nums.

## mod

`(function)`

Usage:

```
(mod num div)
```

Modulus of num and div. Truncates toward negative infinity.

## neg?

`(function)`

Usage:

```
(neg? x)
```

Returns true if x is less than zero, else false.

## next

`(function)`

Usage:

```
(next coll)
```

Returns a seq of the items after the first. Calls seq on its argument. If there are no more items, returns nil.

## nil?

`(function)`

Usage:

```
(nil? x)
```

Returns true if x is nil, false otherwise.

## not

`(function)`

Usage:

```
(not x)
```

Returns true if x is logical false, false otherwise.

## not-any?

`(function)`

Usage:

```
(not-any? pred coll)
```

Returns false if (pred x) is logical true for any x in coll, else true.

## not-empty

`(function)`

Usage:

```
(not-empty coll)
```

If coll is empty, returns nil, else coll,

## not-every?

`(function)`

Usage:

```
(not-every? pred coll)
```

Returns false if (pred x) is logical true for every x in coll, else true.

## not=

`(function)`

Usage:

```
(not= x)
(not= x y & more)
```

Same as (not (= x y))

## nth

`(function)`

Usage:

```
(nth coll index)
(nth coll index not-found)
```

Returns the value at the index. get returns nil if index out of
bounds, nth throws an exception unless not-found is supplied. nth also works for strings.

## odd?

`(function)`

Usage:

```
(odd? n)
```

Returns true if n is odd, throws an exception if n is not an integer.

## or

`(macro)`

Usage:

```
(or)
(or x)
(or x & next)
```

Evaluates exprs one at a time, from left to right. If a form
returns a logical true value, or returns that value and doesn't
evaluate any of the other expressions, otherwise it returns the
value of the last expression. (or) returns nil.

## pos?

`(function)`

Usage:

```
(pos? x)
```

Returns true if x is greater than zero, else false.

## pr-str

`(function)`

Usage:

```
(pr-str)
```

Prints the object(s) to a string. Prints the object(s), separated by spaces if there is more than one. Prints in a way that objects can be read by the reader

## quote

`(special form)`

Usage:

```
(quote form)
```

Yields the unevaluated form.

## rem

`(function)`

Usage:

```
(rem x b)
```

Returns remainder of dividing numerator by denominator.

## remove

`(function)`

Usage:

```
(remove pred coll)
```

Returns a list of the items in coll for which (pred item) returns logical false.

## repeat

`(function)`

Usage:

```
(repeat n x)
```

Returns a list of length n of xs.

## repeatedly

`(function)`

Usage:

```
(repeatedly n f)
```

Takes a function of no args, presumably with side effects, and
   returns an list of n results of calling f.

## reset!

`(function)`

Usage:

```
(reset! atom newval)
```

Sets the value of atom to newval without regard for the current value. Returns newval.

## reverse

`(function)`

Usage:

```
(reverse coll)
```

Returns a seq of the items in coll in reverse order.

## seq

`(function)`

Usage:

```
(seq coll)
```

Returns a seq (list) on the collection. If the collection is empty, returns nil.
(seq nil) returns nil.
seq also works on strings.

## set

`(function)`

Usage:

```
(set coll)
```

Returns a set of the distinct elements of coll.

## set?

`(function)`

Usage:

```
(set? x)
```

Return true if x is a Set

## some

`(function)`

Usage:

```
(some pred coll)
```

Returns the first logical true value of (pred x) for any x in coll,
   else nil.  One common idiom is to use a set as pred, for example
   this will return :fred if :fred is in the sequence, otherwise nil:
   (some #{:fred} coll)

## some->

`(macro)`

Usage:

```
(some-> expr & forms)
```

When expr is not nil, threads it into the first form (via ->),
and when that result is not nil, through the next etc

## some->>

`(macro)`

Usage:

```
(some->> expr & forms)
```

When expr is not nil, threads it into the first form (via ->>),
and when that result is not nil, through the next etc

## some?

`(function)`

Usage:

```
(some? x)
```

Returns true if x is not nil, false otherwise.

## str

`(function)`

Usage:

```
(str)
```

With no args, returns the empty string. With one arg x, returns a string representation of x.
(str nil) returns the empty string.
With more than one arg, returns the concatenation of the str values of the args.

## swap!

`(function)`

Usage:

```
(swap! atom f & args)
```

Atomically swaps the value of atom to be: (apply f current-value-of-atom args).
Returns the value that was swapped in.

## symbol?

`(function)`

Usage:

```
(symbol? x)
```

Return true if x is a Symbol

## take

`(function)`

Usage:

```
(take n coll)
```

Returns a list of the first n items in coll, or all items if there are fewer than n.

## take-while

`(function)`

Usage:

```
(take-while pred coll)
```

Returns a list of successive items from coll while (pred item) returns logical true.

## true?

`(function)`

Usage:

```
(true? x)
```

Returns true if x is true, false otherwise.

## update-vals

`(function)`

Usage:

```
(update-vals m f)
```

Given a map m and a function f of 1-argument, returns a new map where the keys of m
   are mapped to result of applying f to the corresponding values of m.

## vals

`(function)`

Usage:

```
(vals map)
```

Returns a list of the map's values, in the same order as (seq map).

## vec

`(function)`

Usage:

```
(vec coll)
```

Creates a new vector containing the contents of coll.

## vector

`(function)`

Usage:

```
(vector)
```

Creates a new vector containing xs.

## vector?

`(function)`

Usage:

```
(vector? x)
```

Return true if x is a Vector

## when

`(macro)`

Usage:

```
(when test & body)
```

Evaluates test. If logical true, evaluates body in an implicit do.

## when-let

`(macro)`

Usage:

```
(when-let bindings & body)
```

bindings => binding-form test

When test is true, evaluates body with binding-form bound to the value of test

## when-not

`(macro)`

Usage:

```
(when-not test & body)
```

Evaluates test. If logical false, evaluates body in an implicit do.

## while

`(macro)`

Usage:

```
(while test & body)
```

Repeatedly executes body while test expression is true. Presumes
some side-effect will cause test to become false/nil. Returns nil

## zero?

`(function)`

Usage:

```
(zero? x)
```

Returns true if x is zero, else false.