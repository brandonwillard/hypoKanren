(import [hypoKanren.cons [*]])

(require [hy.contrib.walk [let]])


(defn assert-equal [a b]
  (assert (= a b)))

(defn assert-not-equal [a b]
  (assert (!= a b)))

(defn assert-all-equal [&rest tests]
  (reduce (fn [x y] (assert-equal x y) y)
          tests)
  None)

(defn test-cons []
  (assert-all-equal (cons 'a None)
                    (cons 'a [])
                    (cons 'a (,))
                    (cons 'a '())
                    ['a]
                    '(a))
  (assert-all-equal (cons None 'a)
                    (cons [] 'a)
                    (cons (,) 'a)
                    (cons '() 'a))
  (assert-not-equal (cons 'a None)
                    (, 'a))
  (assert-equal (cons 'a None)
                (list (, 'a)))

  (assert-all-equal (cons 'a '(b c))
                    (cons 'a ['b 'c])
                    (cons 'a (, 'b 'c))
                    '(a b c)
                    ['a 'b 'c])
  (assert-not-equal (cons 'a (, 'b 'c))
                    (, 'a))
  (assert-all-equal (cons 'a None)
                    (list (, 'a)))

  (assert-all-equal (cons '(a b) 'c)
                    (cons ['a 'b] 'c)
                    (cons (, 'a 'b) 'c))

  (assert-all-equal (cons '(a b) '(c d))
                    (cons '(a b) ['c 'd])
                    (cons '(a b) (, 'c 'd))
                    [['a 'b] 'c 'd]
                    '((a b) c d))
  (assert-not-equal (cons 'a (, 'b 'c))
                    (, 'a 'b 'c))
  (assert-all-equal (cons 'a (, 'b 'c))
                    (list (, 'a 'b 'c)))
  (assert-all-equal (cons '(a b) 'c)
                    (cons ['a 'b] 'c)
                    (cons (, 'a 'b) 'c))
  (assert-all-equal (cons '(a b) '(c))
                    (cons ['a 'b] ['c])
                    (cons (, 'a 'b) (, 'c))
                    [['a 'b] 'c]
                    '((a b) c))
  (assert-equal (car (cons 'a 'b))
                'a)

  ;; Make sure `cons` will simply chain iterables.
  (assert-equal (type (cons (iter [1 2])
                            (iter [3 4])))
                (type (chain (iter [1 2])
                             (iter [3 4]))))
  (assert-equal (list (cons (iter [1 2])
                            (iter [3 4])))
                (list (cons (iter [1 2])
                            (iter [3 4]))))

  ;; Make sure `car` and `cdr` don't convert some iterable types.
  (let [z (car (cons (iter []) 1))
        expected (iter [])]
    (assert-equal (type z) (type expected))
    (assert-equal (list z) (list expected)))
  (let [z (cdr (cons 1 (iter [])))
        expected (iter [])]
    (assert-equal (type z) (type expected))
    (assert-equal (list z) (list expected)))

  (assert-equal (car (iter [1])) 1)
  (assert-equal (list (cdr (iter [1 2 3]))) [2 3])

  (assert-all-equal (car (cons '(a b) 'a))
                    (car (cons ['a 'b] 'a))
                    (car (cons (, 'a 'b) 'a))
                    ['a 'b]
                    '(a b))
  (assert-not-equal (car (cons (, 'a 'b) 'a))
                    (, 'a 'b))
  (assert-equal (cdr (cons 'a 'b))
                'b)
  (assert-all-equal (cdr (cons 'a None))
                    (cdr (cons '(a) None))
                    (cdr (cons 'a '()))
                    (cdr (cons 'a (,)))
                    (cdr (cons 'a []))
                    [])
  (assert-all-equal (cdr (cons 'a '(b)))
                    (cdr (cons '(a) '(b)))
                    (cdr (cons 'a ['b]))
                    (cdr (cons 'a (, 'b)))
                    ['b]
                    '(b))
  (assert-not-equal (cdr (cons 'a (, 'b)))
                    (, 'b)))
