"Utilities for the creation and use of microKanren constraints.

Example
=======
  => (make-constraint-system [=/= absento symbolo not-pairo]
                             valid-=/=
                             valid-absento
                             valid-symbolo
                             valid-not-pairo)

"
(import [pyrsistent [pmap]])
(import [hypoKanren.cons [cons cdr car]])
(import [hypoKanren.util [*]])
(import [hypoKanren.core [*]])
(import [hypoKanren.unify [*]])

(require [hypoKanren.util [*]])
(require [hypoKanren.core [*]])
(require [hy.contrib.walk [let]])


(defmacro make-constraint-goal-constructor [c-key]
  "Create a function that produces validation goals for a given key.

The function this macro produces takes a set of constrained terms (representing
the constraint operator's rands/arguments) and creates a goal that extends a state
(e.g. `KanrenState`) with those terms when those terms are valid under the
given constraint.

NOTE: The resulting validation goal (produced by the goal constructor--i.e. the
immediate return value) relies on an `invalid?` function defined within the
global namespace. This function, `invalid?`, should take a `KanrenState`
and return a boolean indicating whether or not the constraints are valid.

Example
=======
  => (setv ==c (make-constraint-goal-constructor ==))
  None
  => (take-all
      (call/empty-state (fresh [q] (==c 1 1))))

Parameters
==========
c-key: symbol
  The symbol representing the constraint in a state's constraint store.
  In other words, the key to use in a constraint store.

Returns
=======
out: function
  A function taking constrained terms (e.g. list of `cons`s) and produces a
  validation goal for those terms under the constraint given by `c-key`
"
  `(fn [&rest c-rands]
     ~(.format (+ "Construct a goal for the constraint"
                  " {} and its rands/arguments.")
               (name c-key))
     (require [hypoKanren.core [λₘ]])
     (λₘ [S]
          (do
             ~(.format "Goal for the constraint {}." (name c-key))
             (setv S-new (ext-S S (quote ~c-key) c-rands))
             ;; Check mappings under existing and new constraints.
             (if (invalid? S-new)
                 mzero
                 (unit S-new))))))

(defmacro make-invalid? [cids &rest ps]
  "Creates a single predicate function for a set of constraint symbols and their
individual predicates.

The predicate function takes a `KanrenState` value and returns a boolean
indicating whether or not the constraints are valid.  Unification is included,
by default, under the symbol `==` and predicate `valid-==`.
Empty arguments and body will produce an `invalid?` only for `==`.

Example
=======
  => (setv invalid? (make-invalid? []))

Parameters
==========
cids: list
  List of constraint symbols.  Can be an empty list.
ps: sequence of functions, optional
  Functions used as predicates for validity of the constraints, `cids`, over a
  given state.  Each predicate function must take a single argument for the
  current state, and, if a predicate needs a constraint's current bindings (i.e. terms
  bound by a constraint), it can access them via their symbol value in `cids`.
  XXX: For Python binding to work in that case, the predicate must either be a
  macro, or a lambda defined during the expansion of this macro.

Returns
=======
out: a function that takes a `KanrenState` and returns a boolean.
"
  ;; Let's make sure we aren't adding duplicate constraints.
  (let [cids (list (distinct (+ cids ['==])))
        ;; FIXME: Get unique predicates (without hashing them).
        ;;ps (list (.difference (set ps) [valid-==]))
        ]
    `(fn [S]
       ~(.format "Test validity for the constraints: {}"
                 (.join ", " (lfor x cids (name x))))
       (if (none? S)
           True
           ;; XXX: In this approach, we don't really need to validate `==`, right?
           (not (and (valid-== S) (all ~(lfor p ps (macroexpand `(~p S)))))))
       ;; (require [hy.contrib.walk [let]])
       #_(let [~@(chain.from_iterable
                 (lfor cid cids [cid `(.get S.c-store '~cid [])]))
             s (valid-== S)]
         (cond
           ;; These `(p s)` should pick up the `let` bindings above
           ;; after expansion (if they're macros, of course).
           [(not (none? s)) (any ~(lfor p ps (macroexpand `(~p s))))]
           [True True])))))

(defmacro make-constraint-system [cids &rest ps]
  "Define constraint system variables in the current namespace.

Specifically, define an `invalid?` function, `S0` initial state object,
`==` unification goal, and the constraints `cids` with their corresponding
predicates `ps`.

TODO: Could just create a `KanrenState` object holding these generated values.
Would be better than relying on the local namespace.

Parameters
==========
cids: list
  List of constraint symbols.  Can be an empty list.
ps: sequence of functions, optional
  Functions used as predicates for validity of the constraints, `cids`, over a
  given state.  Must take a single argument for the current state.

Returns
=======
out: generated initial state `KanrenState`
"
  `(do
     (setv invalid? (make-invalid? ~cids ~@ps)
           S0 (make-state (pmap)
                          0
                          (pmap {~@(chain.from_iterable
                                     (lfor c cids [`(quote ~c) '[]]))}))
           == (make-constraint-goal-constructor ==))
     (setv
       ~@(chain.from_iterable
           (lfor cid cids
                 [cid `(make-constraint-goal-constructor ~cid)])))
     S0))

(defn valid-== [S]
  "Check that unification is valid for a list of pairs.

This is different from plain old unification in that its intended use includes
non-lvar terms (e.g. `(cons 1 1)`).  This function can be used to simply confirm
that a list/map of pairs contains *unifiable* terms, whereas actually unifying the
terms would produce a subset containing only (lvar, object) pairs.

Parameters
==========
S: KanrenState

Result
======
out: True or False
"
  (if (none? S)
      False
      (not (none? (fold-unify S.subs)))))

(defn valid-=/= [S]
  "Validity predicate for =/="
  (cond
    [(none? S) False]
    [(in '=/= S.c-store)
     (not (ormap
            (fn [pr]
              (same-s? (car pr) (cdr pr) S.subs))
            (.get S.c-store '=/=)))]
    [True True]))

(defn valid-absento [S]
  "Constraint predicate for `absento`."
  (cond
    [(none? S) False]
    [(in 'absento S.c-store)
     (not (ormap
            (fn [pr]
              (mem? (car pr) (cdr pr) S.subs))
            (.get S.c-store 'absento)))]
    [True True]))

(defn valid-symbolo [S]
  "Constraint predicate for `symbolo`. "
  (cond
    [(none? S) False]
    [(in 'symbolo S.c-store)
     (not (ormap
            ;; Is this just `project`?
            (fn [pr]
              (setv t (walk (car pr) S.subs))
              (not (or (symbol? t) (var? t))))
            (.get S.c-store 'symbolo)))]
    [True True]))

(defn valid-numerico [S]
  "Constraint predicate for `numbero`.

Booleans are *not* considered numbers--in this case.
"
  (cond
    [(none? S) False]
    [(in 'numerico S.c-store)
     (not (ormap
            ;; Is this just `project`?
            (fn [pr]
              (setv t (walk (car pr) S.subs))
              (not (or (and (numeric? t)
                            (not (instance? bool t)))
                       (var? t))))
            (.get S.c-store 'numerico)))]
    [True True]))

(defn valid-booleano [S]
  "Constraint predicate for `booleano`."
  (cond
    [(none? S) False]
    [(in 'booleano S.c-store)
     (not (ormap
            ;; Is this just `project`?
            (fn [pr]
              (setv t (walk (car pr) S.subs))
              (not (or (and (numeric? t)
                            (instance? bool t))
                       (var? t))))
            (.get S.c-store 'booleano)))]
    [True True]))

(defn valid-not-pairo [S]
  "Constraint predicate for `not-pairo` (i.e. not a `cons` pair)."
  (cond
    [(none? S) False]
    [(in 'not-pairo S.c-store)
     (not (ormap
            ;; Is this just `project`?
            (fn [pr]
              ;; XXX: Might want to tell `walk` where to look in the state object.
              ;; Also `n` is a `cons` of constraint rands/arguments/bindings, so keep that in mind.
              (setv t (walk (car pr) S.subs))
              (not (or (not (cons? t)) (var? t))))
            (.get S.c-store 'not-pairo)))]
    [True True]))
