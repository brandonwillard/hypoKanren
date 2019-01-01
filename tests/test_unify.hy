(import [collections [OrderedDict]])
(import [pyrsistent [pmap]])
(import [hy.models [HyList HyExpression]])
(import [hypoKanren.core [var make-state]])
(import [hypoKanren.unify [*]])


(defn test-ext-s []
  (setv test-s (pmap {(var 0) (var 1) (var 1) 'a}))
  (assert (in (var 2) (ext-s (var 2) 'hey test-s)))

  (setv test-s (LVarDAG (pmap {(var 0) (var 1) (var 1) 'a})))
  (assert (= (.get (ext-s (var 2) 'hey test-s) (var 2) )
             'hey)))

(defn test-walk []
  (setv test-no-c (pmap {(var 0) (var 1)
                         (var 1) 'a}))
  (assert (= (walk (var 1) test-no-c)
             (walk (var 0) test-no-c)))
  (assert (= (walk (var 3)
                   (unify [1 (var 3)] [1 'blah] test-no-c))
             'blah))

  ;; Tests for `LVarDAG`'s walk and its cache.
  (setv test-ld (LVarDAG (pmap {(var 0) (var 1)
                                (var 1) 'a})))
  (assert (= (walk (var 1) test-ld)
             (walk (var 0) test-ld)))
  (assert (= (. test-ld walks)
             {(var 1) 'a
              (var 0) 'a}))

  (assert (empty? (. (.ext-s test-ld (var 2) 'b) walks))))

(defn test-unify []
  (assert (= (unify (var 0) 5 (pmap))
             (pmap {(var 0) 5})))

  (assert (= (unify [1 (var 0)] [1 5] (pmap))
             (pmap {(var 0) 5})))
  ;; Unify an improper list
  (assert (= (unify (cons 1 (var 0)) [1 5] (pmap))
             (pmap {(var 0) [5]})))
  (assert (= (unify (cons [1 (var 0)] (var 1)) [[1 2] 5] (pmap))
             (pmap {(var 0) 2
                    (var 1) [5]})))
  ;; Unify within a tuple
  (assert (= (unify (, 1 (var 0)) (, 1 5) (pmap))
             (pmap {(var 0) 5})))
  ;; Unify an improper tuple
  (assert (= (unify (cons 1 (var 0)) (, 1 5) (pmap))
             (pmap {(var 0) (, 5)})))
  ;; Preserve other list-type forms
  (assert (= (unify (cons 1 (var 0)) `[1 5] (pmap))
             (pmap {(var 0) `[5]})))
  (assert (= (unify (cons 1 (var 0)) `(1 5) (pmap))
             (pmap {(var 0) `(5)})))
  ;; Unify within an ordered dict
  (assert (= (unify (OrderedDict {1 (var 0)})
                    (OrderedDict {1 5}) (pmap))
             (pmap {(var 0) 5})))
  ;; Unify an improper ordered dict
  (assert (= (unify (cons (, 1 2) (var 0)) (OrderedDict {1 2 3 4})
                    (pmap))
             (pmap {(var 0) [(, 3 4)]})))

  (assert (none? (unify 5 7 (LVarDAG))))
  (assert (none? (unify (var 0) 2
                        (LVarDAG (pmap {(var 0) 5})))))
  ;; There shouldn't be anything in the cache for this.
  (assert (= (unify 5 5 (LVarDAG))
             (LVarDAG)))
  (assert (= (unify (var 0) 5 (LVarDAG))
             (LVarDAG (pmap {(var 0) 5})
                      (pset [(var 0)])))))

(defn test-fold-unify []
  ;; (assert (none? (fold-unify
  ;;                  [(cons (var 0) 1) (cons 2 3) (cons (var 1) 2) (cons 3 3)])))
  ;; (assert (= (fold-unify [(cons 3 3) (cons (var 0) 1) (cons 2 2) (cons (var 1) 2)])
  ;;            [(cons (var 0) 1)
  ;;             (cons (var 1) 2)]))
  (assert (none? (fold-unify
                   (pmap {(var 0) 1
                          2 3
                          (var 1) 2
                          3 3}))))
  (assert (= (fold-unify (pmap {3 3
                                (var 0) 1
                                2 2
                                (var 1) 2}))
             (pmap {(var 0) 1
                    (var 1) 2})))

  (assert (= (fold-unify (LVarDAG (pmap {3 3
                                         (var 0) 1
                                         2 2
                                         (var 1) 2})))
             (LVarDAG (pmap {(var 0) 1
                             (var 1) 2}))))

  (assert (= (fold-unify (pmap)) (pmap)))
  (assert (= (fold-unify (pmap {1 2})) None))
  (assert (= (fold-unify (pmap {1 1})) (pmap)))
  (assert (= (fold-unify (pmap {(var 0) 1}))
             (pmap {(var 0) 1})))
  (assert (= (fold-unify (pmap [(, 1 1)]))
             (pmap [])))
  (assert (= (fold-unify (pmap [(, 1 1)
                                (, 2 2)]))
             (pmap [])))
  (assert (= (fold-unify (pmap [(, 1 1)
                                (, 1 2)
                                (, 2 2)]))
             None))
  (assert (= (fold-unify (pmap [(, 1 1)
                                (, (var 0) 'a)
                                (, 'b 'b)
                                (, (var 1) (var 2))]))
             (pmap [(, (var 0) 'a)
                    (, (var 1) (var 2))]))))
