"Common miniKanren goals implemented using core microKanren.

"
(import [pyrsistent [pmap]])
(import [hypoKanren.core [*]])
(import [hypoKanren.util [*]])
(import [hypoKanren.unify [*]])

(require [hy.contrib.walk [let]])
(require [hypoKanren.core [*]])
(require [hypoKanren.stream [λₛ]])


(defmacro Zzz [g]
  "Create an inverse-η-delay for a goal.

The actual delay is implemented by λₛ, which simply uses Python's `yield-from`.
"
  `(λₘ [S] (yield-from (~g S))))

(defn pull [$]
  "Extract and evaluate a single value from a lazy state stream."
  (next $))

(defn take-all [$]
  "Extract and evaluate all values from a lazy state stream."
  (list $))

(defn ktake [n $]
  "Take n-many results from a lazy state stream.

NOTE: The 'k' was added to distinguish it from the Hy built-in `take`,
which returns an iterator (and not the list we desire).
"
  (list (take n $)))

(defn conj+ [g0 &rest g-rest]
  "Conjunction of goal streams using inverse-η-delays.

TODO: This could use some form of t.c.o.
"
  (if (empty? g-rest)
      #_(Zzz g0)
      g0
      (conj g0 #_(Zzz g0) (conj+ #* g-rest))))

(defn disj+ [g0 &rest g-rest]
  "Disjunction of goal streams using inverse-η-delays.

TODO: This could use some form of t.c.o.
"
  (if (empty? g-rest)
      g0 #_(Zzz g0)
      (disj g0 #_(Zzz g0) (disj+ #* g-rest))))

(defmacro conde [g-clauses]
  "A goal mirroring `cond`"
  `(disj+ ~@(lfor g g-clauses `(conj+ ~@g))))

(defmacro fresh [lvars &rest body]
  "A goal that introduces fresh lvars.

TODO: This could use some form of t.c.o.
"
  ;; (defn -fresh [lvars body]
  ;;   (if (empty? lvars)
  ;;       `(conj+ ~@body)
  ;;       `(call/fresh (fn [~(first lvars)]
  ;;                      ~(-fresh (list (rest lvars)) body)))))
  ;; (-fresh lvars body)
  (if (empty? lvars)
      `(conj+ ~@body)
      `(call/fresh (fn [~(first lvars)]
                     ;; FIXME: Can't recursively call macros?
                     ;; ~(fresh (rest lvars) body)
                     (fresh ~(list (rest lvars)) ~@body)))))

(defn walk* [v s]
  "Partially reify a form. (replace lvars with their walked values in a form.

By \"partial\", we mean that the actual objects are returned, and not a
representation of them (e.g. strings).

TODO: This could use some form of t.c.o.

Example
=======
=> (walk* [(var 0) [(var 1) (var 2)]]
                  (pmap {(var 0) (var 1)
                          (var 1) 'a
                          (var 2) (var 3)}))
[HySymbol('a'), [HySymbol('a'), <3>]]

Parameters
==========
v: form
  The form, potentially containing lvars, to reify.
s: list of `cons` pairs, `Mapping`
  The substitutions to reify with/against.

Returns
=======
out: The (partially) reified form corresponding to `v`
"
  (let [v (walk v s)]
    (cond
      [(var? v) v]
      [(cons? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [True v])))

(defn reify-state/1st-var [S]
  "Reify only the first variable (i.e. `(var 0)`)."
  (if (all (map symbol? (.keys S.subs)))
      ;; In this case, our mappings are constraint operators to
      ;; the terms bound under them (represented as the operators' rands).
      ;; TODO: Reify based on the operator (e.g. use a reify function
      ;; specific to the operator)
      (do
        S
        #_(let [v (walk* (var 0) (get S.subs '==))]
          (S.new-subs (.set S.subs '== (walk* v (reify-s v (pmap))))))
        #_(let [walk-fn (fn [s]
                          (setv v (walk* (var 0) s))
                          (walk* v (reify-s v (pmap))))]
            (dfor (, c s) (.iteritems S.subs)
                  [c (walk-fn s)])))
      ;; Here, they're simply == substitutions.
      (let [v (walk* (var 0) S.subs)]
        (walk* v (reify-s v (pmap))))))

(defn mK-reify [S*]
  "miniKanren's reification of lvars.

XXX: Only reifies the first lvar.

Parameters
==========
S*: iterable
  Stream of miniKanren states.
"
  (map reify-state/1st-var S*))

(defmacro/g! run [n lvars &rest goals]
  (assert (numeric? n))
  (assert (list? lvars))
  (let [goals (+ (, `(== ~g!q ~(list lvars)))
                 goals)
        lvars (+ [g!q] lvars)]
    `(ktake ~n (mK-reify (call/empty-state
                           (fresh ~lvars ~@goals))))))

(defmacro/g! run* [lvars &rest goals]
  (assert (list? lvars))
  (let [goals (+ (, `(== ~g!q ~(list lvars)))
                 goals)
        lvars (+ [g!q] lvars)]
    `(take-all (mK-reify (call/empty-state
                           (fresh ~lvars ~@goals))))))

;; A goal that's always successful.
(setv s# (== True True))
(setv succeed s#)

;; A goal that always fails.
(setv u# (== False True))
(setv fail u#)
