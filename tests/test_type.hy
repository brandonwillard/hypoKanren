(import pytest)
(import [hypoKanren.cons [cons]])
(import [hypoKanren.goals [*]])
(import [hypoKanren.type [*]])

(require [hypoKanren.goals [*]])
(require [hy.contrib.walk [let]])


;; TODO: Might want to use the pytest timeout plugin and
;; `pytest.mark.timeout(30, method='thread')`.

(defn test-!-o []
  (assert (= (flatten (run 1 [q] (!-o q
                                      `(let [a 1] a)
                                      'int)))
             ["_.0"]))
  (assert (= (flatten (run 1 [q] (!-o []
                                      `(let [f (fn [x] x)]
                                         (f 1))
                                      q)))
             ['int]))

  (assert (= (first (flatten (run 1 [q] (lookupo [`(a :: int) `(b :: bool)] 'a q))))
             'int))
  (assert (= (first (flatten (run 1 [q] (lookupo [`(a :: int) `(b :: bool)] 'b q))))
             'bool))

  (assert (= (flatten (run 1 [q] (!-o [] `(fn [x] 1) q)))
             '("_.0" → int)))

  (assert (= (flatten (run 1 [q] (!-o [] `(fn [x] (+ 1 x)) q)))
             '(int → int)))

  )

#@(pytest.mark.skip
    (defn test-scope []

      ;; FIXME: This broken.
      (assert (= (flatten (run 1 [q] (!-o [`(x :: int)] `(fn [x] x) q)))
                 '(int → int)))
      ;; FIXME: These run indefinitely.
      (assert (= (first (flatten (run 1 [q] (lookupo q 'a 'int))))
                 (cons `[a :: int] "_.0")))
      (assert (= (first (flatten (run 1 [q r] (lookupo q 'a r))))
                 (cons `[a :: "_.0"] "_.1")))

      ;; TODO: There's no relation for empty argument function call forms.
      (assert (= (flatten (run 1 [q] (!-o []
                                          `(let [f (fn [x] 1)]
                                             (f))
                                          q)))))
      ;;
      ;; (run 1 [q] (!-o []
      ;;                 `(let [f (fn [x] x)]
      ;;                    (f (cons (f 5) (f True))))
      ;;                 q))
      ;; ;; '[pair int bool]
      ))
