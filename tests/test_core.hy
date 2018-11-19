(import [pyrsistent [pmap]])
(import [hypoKanren.unify [*]])
(import [hypoKanren.core [*]])


(defn test-== []
  (setv empty-state (make-state (pmap) 0))
  (assert (= (list ((== 1 1) empty-state))
             [(make-state (pmap) 0)]))
  (assert (= (list ((== 1 2) empty-state))
             []))
  (assert (= (next ((== (var 0) 5) empty-state))
             (make-state (pmap {(var 0) 5}) 0))))

(defn test-call/fresh []
  (setv empty-state (make-state (pmap) 0))
  (assert (= (next ((call/fresh
                      (fn [q] (== q 5)))
                    empty-state))
             (make-state (pmap {(var 0) 5}) 1))))

(defn test-ext-S []
  (setv test-state (make-state (pmap [(, 1 2)])
                               0
                               {'=/= [(cons 8 9)]}))
  ;; Add terms to the `==` "constraint" (just unify, really), make sure they're
  ;; in the result--along with the existing constraints.

  (assert (none? (ext-S test-state '== ['a 'b])))

  (assert (= (ext-S test-state '== [(var 0) 'b])
             (make-state (pmap [(, (var 0) 'b) (, 1 2)])
                         0
                         (pmap {'=/= [(cons 8 9)]}))))

  (setv test-state-2 (make-state (pmap [(, 1 2)])
                                 0
                                 (pmap {'=/= [(cons 8 9)]
                                        'absento []})))
  (assert (= (ext-S test-state-2 'absento ['a 'b])
             (make-state (pmap [(, 1 2)])
                         0
                         {'=/= [(cons 8 9)]
                          'absento [(cons 'a 'b)]})))

  ;; TODO: Need to fix `pyrsisent` first.
  #_(assert (= (ext-S test-state-2 'absento ['a 'b])
             (make-state (pmap {'== (pmap {1 2})
                                '=/= (pmap {8 9})
                                'absento (pmap {'a 'b})})
                         0))))
