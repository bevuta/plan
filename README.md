# plan

A `clojure.spec` flavored adaption of
[Prismatic^WPlumatic Graph](https://github.com/plumatic/plumbing).

## Usage

Here's the `stats` example from the Graph README expressed in `plan`:

```clojure
(ns example
  (:require [bevuta.plan :as p]))

(p/defn n [xs] (count xs))
(p/defn m [xs n] (/ (reduce + xs) n))
(p/defn m2 [xs n] (/ (reduce + (map #(* % %) xs)) n))
(p/defn v [m m2] (- m2 (* m m)))

(def stats-plan
  (p/devise `v))

(p/realize stats-plan)
;; => Exception: Missing plan inputs {:missing (example/xs)}

(p/realize stats-plan {`xs [1 2 3 6]})
;; => #:example{xs [1 2 3 6], n 4, m2 25/2, m 3, v 7/2}
```

Step functions are regular functions with positional arguments, so
they can still be used directly. Similar to `clojure.spec`,
`bevuta.plan` keeps a global registry of step dependency
information. `p/defn` is just a convenience macro for declaring
dependencies and defining a function in one go. However, you can also
do it separately, e.g. the `v` definition from the example above could
be expressed like this instead:
 
```clojure
(p/def v :deps [m m2])
(defn v [a b] (- b (* a a)))
```

## Documentation

There's a [basic walk-through](doc/basics.org) that showcases the
essential features of the library. Full API documentation will follow
soon.

## License

Copyright © 2018 bevuta IT GmbH

Distributed under the Eclipse Public License version 1.0, see
[LICENSE]() for full text.
