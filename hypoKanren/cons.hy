(import [collections.abc [Sequence Iterator Sized ByteString]])

(require [hy.contrib.walk [let]])


(defclass ConsPair [object]
  "An object representing cons pairs.

These objects, and the class constructor alias `cons`, serve as a sort of
generalized delayed append operation for various collection types.  When used
with the built-in Python collection types, `cons` behaves like the concatenate
operator between the given types, if any.

A Python `list` is returned when the cdr is a `list` or `None`; otherwise, a
`ConsPair` is returned.

The arguments to `ConsPair` can be a car & cdr pair, or a sequence of objects to
be nested in `cons`es, e.g.

    (ConsPair car-1 car-2 car-3 cdr) == (ConsPair car-1 (cons car-2 (cons car-3 cdr)))
"
  (defn --new-- [cls &rest parts]
    (if (> (len parts) 2)
        (reduce (fn [x y] (ConsPair y x))
                (reversed parts))
        ;; Handle basic car, cdr case.
        (do (setv car-part (-none-to-empty-or-list
                             (first parts)))
            (setv cdr-part (-none-to-empty-or-list
                             (if (and (instance? Sized parts)
                                      (> (len parts) 1))
                                 (last parts)
                                 None)))
            (cond
              [(instance? (, list tuple) cdr-part)
               ;; Try to preserve the exact type
               ;; (e.g. in case it's actually a HyList).
               (+ ((type cdr-part) [car-part]) cdr-part)]
              ;; TODO: Support `OrderedDict` and some form of `set` types.
              [(and (instance? Iterator car-part)
                    (instance? Iterator cdr-part))
               (chain car-part cdr-part)]
              [True
               (do
                 (setv instance (.--new-- (super ConsPair cls) cls))
                 (setv instance.car car-part)
                 (setv instance.cdr cdr-part)
                 instance)]))))
  (defn --hash-- [self]
    (hash [self.car, self.cdr]))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.car other.car)
         (= self.cdr other.cdr)))
  (defn --repr-- [self]
    (.format "({} {} {})"
             (. self __class__ __name__)
             (repr self.car)
             (repr self.cdr)))
  (defn --str-- [self]
    (.format "({} . {})" self.car self.cdr)))

(defn non-str-seq? [x]
  (and (instance? Sequence x)
       (not (instance? (, str ByteString) x))))

;; TODO: Convert to `empty-to-none`
(defn -none-to-empty-or-list [x]
  (cond [(none? x) (list)]
        #_[(non-str-seq? x) (list x)]
        [True x]))

;; A synonym for ConsPair
(setv cons ConsPair)

(defn car [z]
  (cond
    [(hasattr z "car") z.car]
    [(or (instance? Iterator z)
         (non-str-seq? z)) (first z)]
    [(none? z) None]
    [True
     (raise (TypeError (.format "Cannot perform car on {}" (type z))))]))

(defn cdr [z]
  (cond
    [(hasattr z "cdr") z.cdr]
    [(instance? Iterator z) (rest z)]
    [(non-str-seq? z)
     ;; Try to preserve the exact type of collection
     ;; (e.g. in case it's actually a HyList).
     ((type z) (list (rest z)))]
    [(none? z) None]
    [True
     (raise (TypeError (.format "Cannot perform cdr on {}" (type z))))]))

(defn cons? [a]
  (if (or (and #_(instance? list a)
               (non-str-seq? a)
               (not (empty? a)))
          (instance? ConsPair a))
      True
      False))
