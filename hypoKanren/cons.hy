(import [collections [OrderedDict]])
(import [collections.abc [Sequence Iterator Sized ByteString]])

(import [multipledispatch [dispatch]])

(require [hy.contrib.walk [let]])


(defn non-str-seq? [x]
  (and (instance? Sequence x)
       (not (instance? (, str ByteString) x))))

(defn none-to-list [x]
  (cond [(none? x) (list)]
        [True x]))

#@((dispatch object object)
   (defn cons-merge [car-part cdr-part]
     "Merge a generic car and cdr."
     None))

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
        (do (setv car-part (none-to-list
                             (first parts)))
            (setv cdr-part (none-to-list
                             (if (and (instance? Sized parts)
                                      (> (len parts) 1))
                                 (last parts)
                                 None)))
            (or (cons-merge car-part cdr-part)
                (do
                  (setv instance (.--new-- (super ConsPair cls) cls))
                  (setv instance.car car-part)
                  (setv instance.cdr cdr-part)
                  instance)))))
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


;; A synonym for `ConsPair` and the operation of `cons`-ing.
(setv cons ConsPair)


#@((dispatch object ConsPair)
   (defn cons-merge [car-part cdr-part]
     "Merge a cars and cdrs that are `ConsPair`s."
     (ConsPair [car-part (car cdr-part)] (cdr cdr-part))))

#@((dispatch Iterator Iterator)
   (defn cons-merge [car-part cdr-part]
     "Merge a cars and cdrs that are `Iterators`."
     (chain car-part cdr-part)))

#@((dispatch object (, list tuple))
   (defn cons-merge [car-part cdr-part]
     "Merge a car with a list or tuple cdr."
     (+ ((type cdr-part) [car-part]) cdr-part)))

;; TODO: We could broaden this to accept `ConsPair`, so that, in effect, alists
;; are always Python dicts.
#@((dispatch (, list tuple) OrderedDict)
   (defn cons-merge [car-part cdr-part]
     "Merge a list/tuple car with a dict cdr."
     (.update cdr-part [car-part])
     (.move-to-end cdr-part (first car-part) :last False)
     cdr-part))

#@((dispatch (type None))
   (defn car [z] None))

#@((dispatch ConsPair)
   (defn car [z] z.car))

#@((dispatch (, list tuple Iterator))
   (defn car [z]
     (first z)))

#@((dispatch OrderedDict)
   (defn car [z]
     (first (.items z))))

#@((dispatch (type None))
   (defn cdr [z] None))

#@((dispatch ConsPair)
   (defn cdr [z] z.cdr))

#@((dispatch Iterator)
   (defn cdr [z] (rest z)))

#@((dispatch (, list tuple))
   (defn cdr [z]
     ((type z) (list (rest z)))))

#@((dispatch OrderedDict)
   (defn cdr [z]
     (cdr (.items z))))

(defn cons? [a]
  "Determine if an object is the product of a `cons`.

This is automatically determined by the accepted `cdr` types for each `cons-merge`
implementation, since any such implementation implies that `cons` can construct
that type.
"
  (if (and (any (gfor (, c d) (.keys (. cons-merge funcs)) (instance? d a)))
           (or (not (instance? Sized a)) (empty? a)))
      True
      False))
