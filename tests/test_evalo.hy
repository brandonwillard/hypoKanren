(import pytest)
(import [hypoKanren.cons [cons]])
(import [hypoKanren.goals [*]])
(import [hypoKanren.evalo [*]])

(require [hypoKanren.goals [*]])


;; TODO: Add tests.
#@(pytest.mark.skip
    (defn test-evalo []

      (run 1 [q]
           (eval-expo q [] q))

      (run 1 [x y]
           (eval-expo `((fn [y] (cons y ~x)) ~y)
                      []
                      [1]))

      ;; running append in reverse
      (run 1 [x y]
           (eval-expo `((((fn [f]
                            ((fn [x]
                               (f (x x)))
                             (fn [x]
                               (fn [y] ((f (x x)) y)))))
                          (fn [my-append]
                            (fn [l]
                              (fn [s]
                                (if (null? l)
                                    s
                                    (cons (car l) ((my-append (cdr l)) s)))))))
                         (quote ~x))
                        (quote ~y))
                      []
                      ['a 'b 'c 'd 'e]))))
