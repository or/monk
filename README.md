# monk

<!-- badges -->
[![Tests](https://github.com/or/monk/actions/workflows/tests.yml/badge.svg)](https://github.com/or/monk/actions/workflows/tests.yml)
[![Codecov](https://img.shields.io/codecov/c/github/or/monk)](https://app.codecov.io/gh/or/monk)
<!-- /badges -->

> [!WARNING]
> This is work in progress and not yet ready for production.

## About

monk is a code formatter for Clojure/ClojureScript code, aiming to
automatically format code without much configuration, similar to formatters for
other languages, like [gofmt](https://pkg.go.dev/cmd/gofmt) and
[black](https://github.com/psf/black).

### Why monk?

Existing alternatives like [cljfmt](https://github.com/weavejester/cljfmt) and
[cljstyle](https://github.com/greglook/cljstyle) offer flexibility with various
configurations, leading to inconsistent code styles. They also leave a lot of
formatting decisions to the developer, further reducing consistency even inside
a project.

## Who's to say what's right?

Enter Adrian Monk. Who better to ask for tidy, consistent, perfect code?
He'll tell us when things are not properly aligned, when forms don't follow a
pattern, he'll complain about the tiniest wrinkle.

So wherever the formatting leads us: don't blame me, I'm just writing it down.
Monk is the brain behind it all, so no point in arguing, we'll just go with it
and get used to it.

## Tenets

Guiding principles that guide this effort:

1. Strive for consistency
2. Prioritize readability
3. Avoid configuration

I believe two strategies in particular help with that:
- Embrace vertical spacing
- Prefer small diffs when code is changed

## Formatting rules

### Functions

The most frequent form will be that of a function call, in the shape of a list
with a symbol as the first element.

There are three situations that require different formatting:
1. Neither of the argument requires more than one line on their own
   -> All arguments are on the same line
   Example:

    ```clojure
    (println "some text" a-variable-here ", " (a-function-call 1 3 4) " and also a " :keyword)
    ```

2. At least one of the arguments requires more than one line
   -> Add a line break before ALL arguments after the first and align them with
   the first

    ```clojure
    (println "some text"
             a-variable-here
             ", "
             (do
               (play-a-song-or-whatever)
               (a-function-call 1 3 4))
             " and also a "
             :keyword)
    ```

### Blocks

Some forms, usually macros, represent "blocks" of sorts, the formatting should
reflect that. A lot of these forms also expect one or more arguments before the
"block" starts, these should be treated like arguments to functions.
After those N arguments there is ALWAYS a line break to start the block, all
elements part of the block are indented by 2.

Example with 0 arguments:

```clojure
(do
  (do-one-thing)
  (do-another-thing))

; even when it is only one element
(do
  (do-one-thing))
```

Example with 1 argument:

```clojure
(when (suffices-condition? value)
  (do-one-thing)
  (do-another-thing))

; even when it is only one element
(when (suffices-condition? value)
  (do-one-thing))

; EVEN when it is only a single token
(when verified?
  5)

; or two single tokens in the case of 'if'
(if verified?
  5
  -5)
```

Example with 2 arguments:

```clojure
(as-> value x
  (do-first-thing x)
  (do-second-thing x))

; if the first argument needs multiple lines
(as-> (do
        (something)
        (something-else))
      x
  (do-first-thing x)
  (do-second-thing x))
```

#### Special blocks

##### `ns` and its elements
`ns` is a block with one argument, the namespace name. So far so good, but also
its elements that are lists and start with a keyword, e.g. `:require`, `:import`
force a newline after the keyword, aligning all lib specs with that keyword.

```clojure
(ns foo.bar
  "This is a namespace."
  (:require
   [clojure.string :as str]
   [clojure.set :as set])
  (:import
   [some.other.namespace ClassName1 ClassName2]))

; even if it's only one
(ns foo.bar
  (:require
   [clojure.string :as str]))
```

##### `defn`
TBD.

### Paired elements
Another common pattern is that some sibling elements come in pairs, that is
there's always a first element followed by a second element that somehow relates
to the first. For instance the key/value pairs in maps, but also the
conditions and the respective effect in `case`, `cond`, `cond->`, `cond->>`
forms.

The rules here are:
1. Every first element of a pair starts on a new line (except the first), the
   respective second element is separated from it by a single space without a
   new line. Even if the first element requires multiple lines.
2. If at least one pair requires more than one line, then there's a blank line
   between each pair to clearly separate the pairs.

Example:

``` clojure
{:key :value

 (do
   (a-thing)
   (compute-a-key)) :its-value

 :another-key :another-value}

(case value
  :value-1 :result

  :value-2 (do
             (some)
             (bigger)
             (expression))

  :value-3 (another-form))
```

#### Tupled elements
In some rare cases there might even be three elements belonging together.
Currently that's `condp`. In that case they all go on the same line.

The fact that `condp` allows 2 or 3 elements makes this a bit tricky, but at
least it can be determined which case it is.

### Threading
First argument threading changes the effective index of arguments of child
forms, and so it has to be taken into account when making decisions based on the
index of arguments.

`->`, `->>` and their cousins also are block forms, which differs from some
other styles, but makes a lot of sense especially if first argument threading is
honoured while formatting the code, see these examples:

``` clojure
; normal threading form
(-> some-map
  (assoc :key :value)
  (assoc :another :value))

; not code we'd usually write, but here the first argument is a
; -> form as well, so far so consistent
(-> (-> some-map
      (assoc :key :value)
      (assoc :another :value))
  (assoc :key :whatever))

; but if we move the first argument into the parent, as in these examples,
; then it is "missing" in the child form, but note how the block remains
; the same, the code makes it obvious that the first child of the nested
; -> form is NOT the first argument
(-> some-map
  (->
    (assoc :key :value)
    (assoc :another :value))
  (assoc :key :whatever))

(-> some-map
  (assoc :key :whatever)
  (->
    (assoc :key :value)
    (assoc :another :value))
  (assoc :key :whatever))
```
