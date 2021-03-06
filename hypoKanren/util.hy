(import operator)
(import [collections [MutableMapping Mapping Sized]])
(import [pyrsistent [PMap pmap]])
(import [hypoKanren.cons [cons cons? car cdr]])

(require [hy.contrib.walk [let]])
(require [hy.contrib.loop [defnr]])


(defn list? [x]
  (instance? list x))

(defn empty*? [x]
  "Predicate for an empty collection and/or iterable.

Works slightly better than the builtin `empty?`"
  (= (operator.length-hint x 1) 0))

(defn mapping? [x]
  "Predicate for an object implementing the interface `Mapping` (e.g.
a dict).
"
  (instance? Mapping x))

(defn null? [x]
  "Predicate that considers `None`, `False` and an empty collection
to be 'null'.
"
  (or (none? x)
      (= (operator.length-hint x 1) 0)
      (= x False)))

(defmacro get-macro [x]
  "Get a Hy macro (in the current namespace) by name."
  `(get hy.macros._hy_macros __name__ (mangle (quote ~x))))

(defn ormap [pred x]
  "Perform a fold-like `or` over an iterable, `x`, mapped to a predicate, `pred`.

XXX: Not sure if this is guaranteed to be lazily evaluated (e.g. with generators)
and actually short-circuit on the first success.

Maybe use the `first_true` example in `itertools` instead?
"
  (reduce or (map pred x) []))

(defn assp [pred alist]
  "Return only the elements in an association list with `car`s that satisfy
a given predicate; otherwise, return `None`."
  (next (filter (fn [x] (pred (car x))) alist) None))
