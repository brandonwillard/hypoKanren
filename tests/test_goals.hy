(import [pyrsistent [pmap]])
(import [hypoKanren.core [*]])
(import [hypoKanren.goals [*]])

(require [hypoKanren.core [*]])
(require [hypoKanren.goals [*]])


(defn test-fresh []
  (assert (= (pull (call/empty-state
                     (fresh [q r s]
                            (== q 1)
                            (== r s))))
             (make-state (pmap {(var 0) 1
                                (var 1) (var 2)})
                         3))))

(defn test-reify-s []
  (assert (= (reify-s (var 0) (pmap))
             (pmap {(var 0 )
                    ;; TODO: Don't rely on a fixed string representation.
                    "_.0"})))
  #_(assert (= (reify-s (var 0) (pmap {(var 0) (var 1)
                                       (var 1) (cons 'a (var 2))}))
               (pmap {(var 0 ) (var 1)
                      (var 1) (cons 'a (var 2))
                      (var 2) "_.2"}))))

(defn test-walk* []
  ;; TODO FIXME: Tuples should probably work, too.
  ;; (assert (= (walk* (, (var 0) (var 1))
  ;;                   (pmap {(var 0) (var 1)
  ;;                          (var 1) 'a}))
  ;;            (, 'a 'a)))
  (assert (= (walk* [(var 0) [(var 1) (var 2)]]
                    (pmap {(var 0) (var 1)
                           (var 1) 'a
                           (var 2) (var 3)}))
             ['a ['a (var 3)]])))

(defn test-reify-state/1st-var []
  (assert (= (reify-state/1st-var (make-state
                                    (pmap {(var 0) (var 1)
                                           (var 1) 'a})))
             'a))
  (assert (= (reify-state/1st-var (make-state
                                    (pmap {(var 0) (var 1)})))
             ;; TODO: Don't rely on a fixed string representation.
             "_.0"))

  ;; TODO: Add a proper test for reification with constraints!
  #_(assert (state? (reify-state/1st-var
                    (make-state
                      (pmap [(, (var 0) 1)])
                      0
                      (pmap {'=/= [(cons (var 0) (var 1))
                                   (cons (var 1) 2)
                                   (cons 'a 'b)]
                             'absento [[(var 2)]]}))))))

(defn test-conde []
  ;; TODO: Test alternating results from an infinite stream?
  (assert (= (run 4 [r s] (conde
                            [[s# (== r 1) (== s 2)]
                             [s# (== r 3) (== s 4)]]))
             [[1 2] [3 4]])))


(defn test-run* []
  (assert (= (run* [q] (== 1 1))
             ;; TODO: Don't rely on a fixed string representation.
             [["_.0"]]))
  (assert (= (run* [q] (== 2 1))
             []))
  (assert (= (run* [r s t u]
                   (== r u)
                   (== s t))
             ;; TODO: Don't rely on a fixed string representation.
             [["_.0" "_.1" "_.1" "_.0"]])))
