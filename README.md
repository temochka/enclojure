# Enclojure

[![](https://github.com/temochka/enclojure/workflows/Test%20Suite/badge.svg)](https://github.com/temochka/enclojure/actions/workflows/tests.yml)

Enclojure is a Clojure-like scripting language for Elm apps. Enclojure is experimental software and subject to breaking changes.

* [Elm API](https://package.elm-lang.org/packages/temochka/enclojure/latest/)
* [Enclojure API](./API.md)

This is what an Enclojure script looks like:

```clojure
(defn metal?
  [song-info]
  (let [{:keys [tags]} song-info]
    (some #(string/includes? (string/lower-case %) "metal") tags)))

(->> [{:artist "Pallbearer" :title "I Saw The End" :tags #{"metal" "doom metal"}}
      {:artist "Baroness" :title "Tourniquet" :tags #{"metal" "hard-rock"}}
      {:artist "Jakob Bro" :title "Giant" :tags #{"jazz" "scandinavian jazz"}}]
     (filter metal?)
     count)
```

## Feature highlights

- ⭐️ **Clojure look'n'feel.** Supports familiar Clojure features: special form syntax, data literals, destructuring. Comes with a subset of functions from `clojure.core` and `clojure.string`.
- ⭐️ **BYO side effects.** An Enclojure function can be configured to produce an arbitrary Elm command. This can be used to script the UI or send HTTP requests from scripts.
- ⭐️ **Eval doesn’t block the render.** A computation budget can be dedicated to eval on every frame. This allows the app to put the interpreter on hold to handle other events or avoid deadlocks due to programming mistakes
- ⭐️ **Synchronous execution model.** Similarly to JVM Clojure (and unlike JavaScript and ClojureScript), all side-effecting functions block the interpreter until the result is available, which leads to simpler programs.
- ⭐️ **Written in pure Elm with minimal dependencies.** Can be installed as a package, integrates seamlessly into an existing Elm app.

## Differences from Clojure

* Not lazy: all functions execute eagerly, “lazy” arities (e.g., `(range)`, `(repeat x)`) of core functions are not implemented.
* No agents or refs: similarly to ClojureScript, only atoms and vars are supported.
* No multimethods (yet?).
* No syntax quote or user macros (yet?).
* No `:or` when destructuring (yet?).
* No namespaces or namespaced keywords.
* No character value type: a single-char string is returned wherever Clojure would return a character.
* No metadata.
* No protocols, records, types.
* No annotation syntax.
* No reader tags.
* No transducers.
* No `catch`: to discourage exception-based control flow and treat exceptions as panic.
* No `loop`: any function can be recursive without overflowing the stack.
* Only integers and floats are supported: ratios, big integers, long, etc. are out of scope.
* Every `cond` is a `cond-let`.
* No watch functions on atoms.
* No array-map, sorted-map, sorted-set.
