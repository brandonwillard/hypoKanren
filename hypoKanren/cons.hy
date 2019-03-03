(import [operator [length-hint]])
(import [collections [OrderedDict]])
(import [collections.abc [Sequence Iterator ByteString]])

(import [multipledispatch [dispatch]])

(require [hy.contrib.walk [let]])


(defn non-str-seq? [x]
  (and (instance? Sequence x)
       (not (instance? (, str ByteString) x))))

(defclass ConsType [type]
  (defn --instancecheck-- [self o]
    (cons? o)))

(defclass ConsPair [object :metaclass ConsType]
  "An object representing `cons` pairs.

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
  (setv __slots__ ["car" "cdr"])

  (defn --new-- [cls &rest parts]
    (cond
      ;; Handle `(1 2 ... n . (inc n))`
      [(> (len parts) 2)
       (reduce (fn [x y] (ConsPair y x))
               (reversed parts))]
      ;; Handle basic car, cdr case.
      [(= (len parts) 2)
       (do
         (setv car-part (first parts))
         (setv cdr-part (last parts))
         (try
           (cons-merge car-part cdr-part)
           (except [NotImplementedError]
             (do
               (setv instance (.--new-- (super ConsPair cls) cls))
               (setv instance.car car-part)
               (setv instance.cdr cdr-part)
               instance))))]
      [True
       (raise (ValueError "Number of arguments must be greater than 2."))]))
  (defn --hash-- [self]
    (hash [self.car self.cdr]))
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

#@((dispatch object (type None))
   (defn cons-merge [car-part cdr-part]
     "Merge a generic car and cdr.

    This is the base/`nil` case with `cdr` `None`; it produces a standard Python list.
"
     [car-part]))

#@((dispatch object ConsPair)
   (defn cons-merge [car-part cdr-part]
     "Merge a car and a `ConsPair` cdr."
     (ConsPair [car-part (car cdr-part)] (cdr cdr-part))))

#@((dispatch object Iterator)
   (defn cons-merge [car-part cdr-part]
     "Merge a car and an `Iterator` cdr."
     (chain [car-part] cdr-part)))

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
     (cdr (list (.items z)))))

(defn cons? [a]
  "Determine if an object is the product of a `cons`.

This is automatically determined by the accepted `cdr` types for each
`cons-merge` implementation, since any such implementation implies that `cons`
can construct that type.
"
  (or (issubclass (type a) ConsPair)
      (and
        ;; The `cdr`s in our implemented `cons-merge` functions
        (any (gfor (, _ d) (.keys (. cons-merge funcs))
                   :if (not (in d (, object ConsPair)))
                   (instance? d a)))
        ;; It can't be empty
        (> (length-hint a 0) 0))))

(defn null? [a]
  "Check if an object is null according to `cons` semantics.

    A null object is one that can be used as a `cdr` to produce a
 non-`ConsPair` collection (e.g. `None`, `[]`, `()`, `OrderedDict`, etc.)

    It's important that this function be used when considering an arbitrary
 object as the terminating `cdr` for a given collection (e.g. when unifying
 `cons` objects); otherwise, fixed choices for the terminating `cdr`, such as
 `None` or `[]`, will severely limit the applicability of the decomposition.

    Also, for relevant collections with no concrete length information, `None`
 is returned, and it signifies the uncertainty of the negative assertion.
"
  (cond
    [(none? a) True]
    [(any (gfor k (.keys (. cdr funcs))
                :if (not (issubclass (first k) (, (type None) ConsPair)))
                (instance? (first k) a)))
     (let [lhint (length-hint a -1)]
       (cond
         [(= lhint 0) True]
         [(> lhint 0) False]
         [True None]))]
    [True False]))
