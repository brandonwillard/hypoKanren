(import [collections [OrderedDict]])
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
  ;; Make sure `cons` preserves collection types.
  (assert-all-equal (cons 'a None)
                    (cons 'a [])
                    ['a])
  (assert-equal (cons 'a (,)) (, 'a))
  (assert-equal (cons 'a '()) '(a))

  (assert-all-equal (cons None 'a)
                    (cons [] 'a)
                    (ConsPair [] 'a))

  (assert-equal (cons (,) 'a)
                (ConsPair (,) 'a))
  (assert-equal (cons '() 'a)
                (ConsPair '() 'a))
  (assert-equal (cons 'a None)
                ['a])

  (assert-equal (cons 'a '(b c))
                '(a b c))
  (assert-equal (cons 'a ['b 'c])
                ['a 'b 'c])
  (assert-equal (cons 'a (, 'b 'c))
                (, 'a 'b 'c))
  (assert-equal (type (cons (, 'a 1) {'b 2}))
                ConsPair)
  (assert-equal (cons (, 'a 1) (OrderedDict {'b 2}))
                (OrderedDict {'a 1 'b 2}))

  (assert-equal (cons '(a b) 'c)
                (ConsPair '(a b) 'c))
  (assert-equal (cons ['a 'b] 'c)
                (ConsPair ['a 'b] 'c))
  (assert-equal (cons (, 'a 'b) 'c)
                (ConsPair (, 'a 'b) 'c))

  (assert-equal (cons '(a b) '(c d))
                '((a b) c d))
  (assert-equal (cons ['a 'b] ['c 'd])
                [['a 'b] 'c 'd])
  (assert-equal (cons '(a b) ['c 'd])
                ['(a b) 'c 'd])
  (assert-equal (cons (, 'a 'b) ['c 'd])
                [(, 'a 'b) 'c 'd])
  (assert-equal (cons '(a b) (, 'c 'd))
                (, '(a b) 'c 'd))
  (assert-equal (cons ['a 'b] (, 'c 'd))
                (, ['a 'b] 'c 'd))

  (assert-equal (car (cons 'a 'b)) 'a)

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

  (assert-equal (car (cons '(a b) 'a))
                '(a b))
  (assert-equal (car (cons ['a 'b] 'a))
                ['a 'b])
  (assert-equal (car (cons (, 'a 'b) 'a))
                (, 'a 'b))

  (assert-equal (cdr (cons 'a 'b))
                'b)
  (assert-all-equal (cdr (cons 'a None))
                    (cdr (cons '(a) None))
                    [])

  (assert-equal (cdr (cons 'a '()))
                '())
  (assert-equal (cdr (cons 'a (,)))
                (,))
  (assert-equal (cdr (cons 'a []))
                [])
  (assert-equal (cdr (cons 'a '(b)))
                '(b))
  (assert-equal (cdr (cons 'a (, 'b)))
                (, 'b))
  (assert-equal (cdr (cons 'a ['b]))
                ['b])

  (assert-equal (cons 'a (cons 'b 'c))
                (cons ['a 'b] 'c))
  (assert-equal (cons (cons 'a 'b) (cons 'c 'd))
                (cons [(cons 'a 'b) 'c] 'd)))
