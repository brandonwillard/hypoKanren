(import [pyrsistent [pmap]])
(import [itertools [count cycle]])
(import [hypoKanren.stream [*]])

(require [hypoKanren.stream [λₛ]])
(require [hypoKanren.stream [*]])


(defn test-interleave-inf []
  (assert (= (list (take 5 (interleave-inf (cycle [[1 2] [3 4]]))))
             [1 3 1 3 1])))

(defn test-mplus []
  (assert (= (list (mplus [1] [2 3])) [1 2 3]))
  (assert (= (list (mplus (λₛ [] [1]) [2 3]))
             [1 2 3]))
  (assert (= (list (mplus (λₛ [] [1]) (λₛ [] [2 3])))
             [1 2 3])))

(defn test-bind []
  (assert (= (list (bind ['state-1 'state-2]
                         (fn [state]
                           [(.format "{} succeeded!" state)])))
             ["state-1 succeeded!" "state-2 succeeded!"]))

  (assert (= (list (bind (λₛ [] ['state-1 'state-2])
                         (fn [state]
                           [(.format "{} succeeded!" state)])))
             ["state-1 succeeded!" "state-2 succeeded!"]))

  (defn test-goal [state]
    (if (even? state)
        ;; This state fails.
        []
        [(.format "{} succeeded!" state) (+ state 1)]))

  (assert (= (set (bind [1 2 3 4 5 6] test-goal))
             (set ["1 succeeded!" "3 succeeded!" "5 succeeded!"
                   2 4 6])))

  (assert (= (list (take 3 (bind (count) test-goal)))
             ["1 succeeded!" "3 succeeded!" "5 succeeded!"])))

(defn test-inverse-η-delays []
  "Examples taken from Heman and Friedman, µKanren (2013)."
  (import [hypoKanren.core [var make-state disj == call/fresh]])
  (require [hypoKanren.core [λₘ]])
  (require [hypoKanren.goals [Zzz]])

  (defn fives [x]
    (disj (== x 5) (fives x)))

  (defn sixes [x]
    (disj (== x 6)
          (Zzz (sixes x))))

  (setv fives-and-sixes
        (call/fresh (fn [x] (disj (fives x) (sixes x)))))

  (setv empty-state (make-state))

  ;; XXX: Won't work; maximum recursion depth.
  (assert
    (try
      (do
        (fives-and-sixes empty-state))
      (except [RecursionError]
        True)
      (else
        False)))

  (defn fives [x]
    (disj (== x 5) (Zzz (fives x))))

  (setv res-stream (fives-and-sixes empty-state))
  (assert (= (next res-stream)
             (make-state (pmap {(var 0) 5}) 1)))
  (assert (= (next res-stream)
             (make-state (pmap {(var 0) 6}) 1)))
  (assert (= (next res-stream)
             (make-state (pmap {(var 0) 5}) 1))))
