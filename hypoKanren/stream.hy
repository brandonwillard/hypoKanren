"Objects, functions and macros relating to the creation and use of microKanren
streams.

Streams are simply Python iterators and/or generator functions.
"
(import [itertools [cycle takewhile]])
(import [functools [partial]])

(require [hy.contrib.walk [let]])


(defmacro λₛ [args body]
  "A delayed/immature stream creation macro.

Use this to construct a delayed/immature stream from an existing stream.

In the base case, `args` are immaterial and the body must evaluate to a state
stream.
"
  `(iter ~body))

;; TODO: Should this be a macro, or function yielding `[]`, like `unit`?
;; The object representing an empty Kanren state stream.
(setv mzero (λₛ [] []))

(defn unit [S]
  "Initialize a state stream with a state instance."
  (yield S))

(defn interleave-inf [seqs]
  "A version of `interleave` that more gracefully handles unequally sized and
possibly infinite iterables.

Hy's built-in `interleave` does not interleave all iterables in an unequally
sized sequence, like `zip_longest` does, and `zip_longest` doesn't appear to
handle infinite iterables.

This is similar to `roundrobin` in the `itertools` doc and an adaptation of
`interleave` in `toolz`.
"
  (let [iters (cycle (map iter seqs))
        predicate None]
    (while True
      (try
        (for [itr iters]
          (yield (next itr)))
        (return)
        (except [StopIteration]
          (setv predicate (partial not? itr))
          (setv iters (cycle (takewhile predicate iters))))))))

(defn mplus [$1 $2]
  "Append streams of (possibly delayed) states/substitutions."
  (interleave-inf [$1 $2]))

(defn bind [$ g]
  "Add a goal to a stream."
  (interleave-inf (map g $)))
