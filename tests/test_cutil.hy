(import [pyrsistent [pmap]])
(import [hypoKanren.cons [cons]])
(import [hypoKanren.core [var make-state]])
(import [hypoKanren.cutil [*]])
(import [hypoKanren.unify [LVarDAG]])

(require [hypoKanren.cutil [*]])
(require [hy.contrib.walk [let]])


(defn test-valid-== []
  ;; This is mostly covered by `test-fold-unify`.
  (assert (not (valid-== (make-state (pmap {(var 0) 3
                                              1 1
                                              1 2})))))
  (assert (valid-== (make-state (pmap [(, 2 2)
                                       (, (var 1) 2)]))))
  (assert (valid-== (make-state)))
  (assert (valid-== (make-state (pmap {(var 0) 3
                                       1 1
                                       2 2})))))

(defn test-make-constraint-goal-constructor []

  ;; Test using unification constraint.
  (defn invalid? [S]
    (not (valid-== S)))

  ;; (invalid? (ext-S (make-state) '== (, 1 2)))

  (setv ==c (make-constraint-goal-constructor ==))

  ;; Construct a goal for existing (invalid) constraints, and make
  ;; sure the resulting goal returns an empty stream no matter what.
  (assert (= (call/empty-state (==c 1 2))
             mzero))

  ;; Construct a goal for valid unification constraints, but evaluate it
  ;; on an empty state.
  (assert (= (next (call/empty-state (==c 2 2)))
             (make-state)))

  ;; Construct a goal for valid unification constraints, and evaluate it
  ;; on a non-empty state with an existing invalid mapping.
  (setv test-state-1 (make-state (pmap [(, 1 2)])))
  (assert (= ((==c 2 2) test-state-1)
             mzero))
  ;; There's an existing constraint that's already invalid, so, although
  ;; this goal should succeed, the entire state should fail!
  (assert (= ((==c (var 1) 2) test-state-1)
             mzero))

  ;; Construct a goal for valid unification constraints, and evaluate it
  ;; on a non-empty state with an existing invalid mapping.
  (setv test-state-2 (make-state (pmap [(, 2 2)])))
  (assert (= (next ((==c (var 1) 2) test-state-2))
             (make-state (pmap [(, (var 1) 2)
                                (, 2 2)]) 0))))

(defn test-make-invalid? []
  ;; Redundant, because `==` is already in there by default.
  (setv invalid? (make-invalid? [==] valid-==))
  (setv ==c (make-constraint-goal-constructor ==))

  (assert (= (next (call/empty-state (==c 2 2)))
             (make-state)))
  (assert (= (call/empty-state (==c 1 2))
             mzero)))

(defn test-make-constraint-system []
  ;; Defines variables in the active namespace.
  (setv test-state (make-constraint-system []))
  (assert (in 'S0 (locals)))
  ;; (assert (in '== S0.subs))
  (assert (in (mangle '==) (locals)))
  (assert (in (mangle 'invalid?) (locals)))
  (del S0 == invalid?)

  ;; (macroexpand '(make-constraint-system [=/=] (fn [S] S)))

  (setv test-state (make-constraint-system [=/=] valid-=/=))
  (assert (in 'S0 (locals)))
  ;; (assert (in '== S0))
  ;; (assert (in '=/= S0))
  (assert (in (mangle '==) (locals)))
  (assert (in (mangle '=/=) (locals)))
  (assert (in (mangle 'invalid?) (locals))))

(defn test-=/= []
  (setv invalid? (make-invalid? [=/=] valid-=/=))

  ;; This state unifies an lvar that `=/=` says it shouldn't.
  (setv test-state (make-state (pmap [(, (var 0) 2)])
                               0
                               (pmap  {'=/= [(cons (var 0) 2)]})))
  (assert (invalid? test-state))
  ;; This state unifies an lvar that `=/=` constrains, but the unification
  ;; doesn't violate the constraint.
  (setv test-state (make-state (pmap [(, (var 0) 2)])
                               0
                               (pmap {'=/= [(cons (var 0) 3)]})))
  (assert (not (invalid? test-state)))

  ;; A constraint, but nothing to conflict with.
  (setv test-state (make-state (pmap)
                               0
                               (pmap {'=/= [(cons (var 0) 3)]})))
  (assert (not (invalid? test-state)))

  ;; No constraints
  (setv test-state (make-state (pmap [(, (var 0) 3)])
                               0
                               (pmap {'=/= []})))
  (assert (not (invalid? test-state)))

  ;; Nothing at all.
  (setv test-state (make-state (pmap)
                               0
                               (pmap {'=/= []})))
  (assert (not (invalid? test-state)))

  ;; A degenerate constraint (always invalid)
  (setv test-state (make-state (pmap [(, (var 0) 2)])
                               0
                               (pmap {'=/= [(cons 3 3)]})))
  (assert (invalid? test-state))

  ;; A degenerate constraint (always valid) with an invalid state.
  (setv test-state (make-state (pmap [(, 3 2)])
                               0
                               (pmap {'=/= [(cons 1 3)]})))
  (assert (invalid? test-state)))
