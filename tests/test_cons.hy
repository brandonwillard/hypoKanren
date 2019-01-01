(import [collections [Iterable OrderedDict]])
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

  (assert-equal (. (cons None 'a) car) None)
  (assert-equal (. (cons None 'a) cdr) 'a)

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

  ;; Make sure `cons` will simply chain iterables.
  (assert-equal (type (cons 1 (iter [3 4])))
                chain)
  (assert-equal (list (cons [1 2]
                            (iter [3 4])))
                [[1 2] 3 4])
  (assert-equal (list (cons 1 (iter [2 3])))
                [1 2 3])
  (assert-equal (cons 'a (cons 'b 'c))
                (cons ['a 'b] 'c))
  (assert-equal (cons (cons 'a 'b) (cons 'c 'd))
                (cons [(cons 'a 'b) 'c] 'd)))

(defn test-car-cdr []

  (assert-equal (car (cons 'a 'b)) 'a)

  ;; Make sure `car` and `cdr` don't convert some iterable types.
  (let [z (car (cons (iter []) 1))
        expected (iter [])]
    (assert-equal (type z) (type expected))
    (assert-equal (list z) (list expected)))
  (let [z (cdr (cons 1 (iter [])))
        expected (iter [])]
    (assert (instance? Iterable z))
    (assert-equal (list z) (list expected)))

  (assert-equal (car (iter [1])) 1)
  (assert-equal (list (cdr (iter [1]))) [])
  (assert-equal (list (cons (car (iter [1]))
                            (cdr (iter [1]))))
                [1])

  (assert-equal (list (cdr (iter [1 2 3])))
                [2 3])

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

  (assert-equal (car (OrderedDict {1 2 3 4}))
                (, 1 2))
  (assert-equal (cdr (OrderedDict {1 2 3 4}))
                [(, 3 4)])
  (assert-equal (cdr (OrderedDict {1 2}))
                []))

(defn test-cons? []
  (assert (cons? (cons 1 "hi")))
  (assert (cons? (, 1 2)))
  (assert (cons? [1 2]))
  (assert (cons? (OrderedDict {1 2})))
  (assert (cons? (iter [1])))
  ;; Non-cons by type
  (assert (not (cons? {})))
  (assert (not (cons? (set))))
  (assert (not (cons? (set [1 2]))))
  (assert (not (cons? 'hi)))
  (assert (not (cons? "hi")))
  (assert (not (cons? 1)))
  ;; Non-cons because empty
  (assert (not (cons? (iter []))))
  (assert (not (cons? (OrderedDict {}))))
  (assert (not (cons? (, ))))
  (assert (not (cons? []))))


(defn test-null? []
  (assert (null? None))
  (assert (null? []))
  (assert (null? (,)))
  (assert (null? (OrderedDict)))
  (assert (null? (iter [])))
  (assert (not (null? object)))
  (assert (not (null? [1])))
  (assert (not (null? (, 1))))
  (assert (not (null? (OrderedDict [(, 1 2)]))))
  (assert (not (null? (iter [1]))))
  (assert (not (null? (cycle [5])))))
