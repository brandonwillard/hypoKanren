(import [hypoKanren.cons [cons]])
(import [pyrsistent [pmap]])
(import [hypoKanren.core [call/fresh call/empty-state var make-state]])
(import [hypoKanren.cutil [*]])

(require [hypoKanren.cutil [*]])
(require [hy.contrib.walk [let]])


(defn test-constraints []
  (make-constraint-system [=/= absento symbolo not-pairo]
                          valid-=/=
                          valid-absento
                          valid-symbolo
                          valid-not-pairo)

  (assert (= (. (next (call/empty-state
                              (call/fresh
                                (fn [q]
                                  (== `(~q 1 2) `(+ 1 2))))))
                      subs data)
             (pmap {(var 0) '+})))

  (assert (= (list (call/empty-state
                     (call/fresh
                       (fn [x]
                         (conj
                           (== 'b x)
                           (absento 'b [1 (cons 'c x)]))))))
             []))
  (assert (= (.get (. (next (call/empty-state
                              (call/fresh
                                (fn [x]
                                  (conj
                                    (== 'a x)
                                    (absento 'b [1 (cons 'c x)]))))))
                      c-store)
                   "absento")
             [['b 1 (cons 'c (var 0))]]))

  (assert (= (list (call/empty-state
                     (call/fresh
                       (fn [x]
                         (conj
                           (== 1 x)
                           (symbolo x))))))
             []))
  (assert (= (.get (. (next (call/empty-state
                              (call/fresh
                                (fn [x]
                                  (conj
                                    (== 'a x)
                                    (symbolo x))))))
                      c-store)
                   "symbolo")
             [[(var 0)]]))

  (assert (= (list (call/empty-state
                     (call/fresh
                       (fn [x]
                         (conj
                           (== [1 2 3] x)
                           (not-pairo x))))))
             []))
  (assert (= (list (call/empty-state
                     (call/fresh
                       (fn [x]
                         (conj
                           (== x (cons 1 2))
                           (not-pairo x))))))
             []))
  (assert (= (list (call/empty-state
                     (call/fresh
                       (fn [x]
                         (conj
                           (== x (cons 1 2))
                           (not-pairo x))))))
             []))

  (let [res
        (list (call/empty-state
                (call/fresh
                  (fn [x]
                    (reduce conj
                            [(== 'a x)
                             (=/= x 'b)
                             (absento 'b [x])
                             (not-pairo x)
                             (symbolo x)
                             (=/= 'c x)])))))
        res-state (first res)]
    res-state
    (assert (= 1 (len res)))
    (assert (= res-state.subs.data
               (pmap {(var 0) 'a})))
    (assert (= (get res-state.c-store "=/=")
               [(cons 'c (var 0))
                (cons (var 0) 'b)]))
    (assert (= (get res-state.c-store "absento")
               [['b (var 0)]]))
    (assert (= (get res-state.c-store "symbolo")
               [[(var 0)]]))
    (assert (= (get res-state.c-store "not-pairo")
               [[(var 0)]]))))
