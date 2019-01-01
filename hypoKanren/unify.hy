"Functions and macros relating to Kanren objects and generic
unification of Hy forms.
"
(import [multipledispatch [dispatch]])
(import [collections [MutableMapping Mapping UserDict]])
(import [pyrsistent [PMap pmap pset]])
(import [hypoKanren.cons [cons cons? car cdr]])
(import [hypoKanren.util [mapping? null?]])

(require [hy.contrib.walk [let]])
;; (require [hy.contrib.loop [defnr]])


(defclass LVar [object]
  (setv unassigned (gensym))
  (setv __slots__ ["id"])
  (defn --init-- [self id]
    (setv self.id id))
  (defn --repr-- [self]
    (.format "({} {})"
             (. self __class__ __name__)
             (repr self.id)))
  (defn --str-- [self]
    (.format "_.{}" self.id))
  (defn --hash-- [self]
    (hash self.id))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.id other.id))))

(defn var [c]
  "Logic variable constructor.

Logic variables bind to any form during unification.

Parameters
==========
c: int
  An integer identifying the logic variable.
"
  (assert (integer? c))
  (LVar c))

(defn var? [x]
  (instance? LVar x))

(defclass LVarDAG [UserDict]
  "Object representing LVar substitutions."
  (setv __slots__ ["data" "lvars" "walks"])
  (defn --init-- [self &optional [subs None] [lvars None] [walks None]]
    (setv self.data (if (none? subs) (pmap) subs))
    (setv self.lvars (if (none? lvars)
                         (pset (filter var? (.keys self.data)))
                         lvars))
    (setv self.walks (if (none? walks) {} walks)))
  (defn ext-s [self from to]
    ;; TODO: We should be able to incrementally extend the cache (in some
    ;; cases).
    (LVarDAG
      (.set self.data from to)
      ;; TODO: This might be a little too limiting; walk for all lvars (in both
      ;; `from` and `to`)?  For now, we
      ;; It's actually not necessary at all when/if all keys are lvars.
      (if (var? from)
          (.add self.lvars from)
          self.lvars)
      ;; Reset the cache.
      {}))
  (defn walk [self u]
    (if (var? u)
        (.setdefault self.walks u
          (walk u self.data))
        (walk u self.data)))
  (defn get [self u &optional default]
    (.get self.data u default))
  (defn --repr-- [self]
    (.format "({} {} {} {})"
             (. self __class__ __name__)
             (repr self.data)
             (repr self.lvars)
             (repr self.walks)))
  (defn --str-- [self]
    (str self.data))
  (defn --hash-- [self]
    (hash (, self.data self.lvars)))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.data other.data)
         (= self.lvars other.lvars))))

#@((dispatch object object list)
   (defn ext-s [u v s]
     "Extend a substitution list, `s`, by adding a new substitution (i.e. the pair
`(u, v)`).

This function handles alists (i.e. `list`s with `cons` pairs) and `Mapping`s.

TODO: Consider using dispatch for input-type overloading.

Parameters
==========
u: form
  First element/key of new pair/substitution.
v: form
  Second element/value of new pair/substitution.
s: list of `cons` cells or `Mapping`
  Existing pairs/substitutions.

Results
=======
out: `s` with new pair/substitution.
"
     (cons (cons u v) s)))

#@((dispatch object object MutableMapping)
   (defn ext-s [u v s]
     (doto s (assoc u v))))

#@((dispatch object object PMap)
   (defn ext-s [u v s]
     (.set s u v)))

#@((dispatch object object LVarDAG)
   (defn ext-s [u v s]
     (.ext-s s u v)))

#@((dispatch object Mapping)
   (defn walk [u s]
     "Get the substitution value for a given logic variable, if any.

This function handles `Mapping`s.

TODO: Tail-call optimization.

Examples
========
  ;; No existing substitutions.
  => (walk (var 1) {})
  <1>
  ;; Existing substitutions, but not for `u`.
  => (walk (var 1) {(var 0) 'q})
  <1>
  ;; Existing substitution for `u`.
  => (walk (var 1) {(var 1) 'q})
  HySymbol('q')

Parameters
==========
u: HySymbol
  Symbol for which we want the substitution value.
s: Mapping
  Existing substitutions.

Results
=======
out: symbol
"
     (if (var? u)
         (let [val (.get s u None)]
           (if (null? val)
               u
               (walk val s)))
         u)))

#@((dispatch object LVarDAG)
   (defn walk [u s]
     (.walk s u)))

#@((dispatch object object object)
   (defn unify [u v s]
     "Unify terms given a set of existing substitutions.

TODO: Consider using dispatch for input-type overloading.
TODO: Make this amenable to tail-call optimization.

Parameters
==========
u: form
  First form to unify with the second form.
v: form
  Second form to unify with the first form.
s: list of `cons` cells or `Mapping`
  Existing substitutions.

Results
=======
out: `None` if terms cannot be unified; otherwise, `s` or, when `u` is an
`lvar`, `s` plus the pair `u` and `v`.
"
     (let [u (walk u s)
           v (walk v s)]
       (cond
         [(and (var? u) (var? v) (= u v)) s]
         [(var? u) (ext-s u v s)]
         [(var? v) (ext-s v u s)]
         ;; TODO: This currently won't unify some standard Python/Hy objects like
         ;; tuples, dicts, sets.
         [(and (cons? u) (cons? v))
          ;; TODO: This branch point needs to change if we want to use tail-call
          ;; optimization.  If `defnr`'s approach didn't use the same global
          ;; variables for every call to the `defnr` function, we could have some
          ;; form of (partial?) t.c.o.
          (let [s (unify (car u) (car v) s)]
            (if (none? s)
                None
                (unify (cdr u) (cdr v) s)))]
         [(= u v) s]
         [True None]))))

(defn same-s? [u v s]
  "Do the substitutions `s` change after unifying `u` and `v` under `s`?"
  (= (unify u v s) s))

#@((dispatch Mapping)
   (defn fold-unify [s]
     "Short-circuiting fold/reduce-ing unify."
     (setv lcdr cdr)
     (setv s (.iteritems s))
     ;; TODO: When `s` is a `Mapping` can't we simply check whether or not an
     ;; item is in there?  Perhaps only the non-lvar terms need to be checked
     ;; for unification?
     (setv s0 (pmap))
     (setv running-s s0)
     (for [pr s]
       (do
         (setv running-s (unify (car pr)
                                (last pr)
                                running-s))
         (if (none? running-s)
             (return None))))
     running-s))

#@((dispatch LVarDAG)
   (defn fold-unify [s]
     (let [s-new (fold-unify s.data)]
       (if (not (none? s-new))
           (LVarDAG s-new)
           None))))

#@((dispatch object object object)
   (defn mem? [u v s]
     "Check if a term `u` is already equivalent to any subterm of a term `v`
under substitution `s`.

TODO: Tail-call optimization.
"
     (let [v (walk v s)]
       (or (same-s? u v s)
           (and (cons? v)
                (or (mem? u (car v) s)
                    (mem? u (cdr v) s)))))))

;; TODO: Define a reify taking state objects.  It should pass through all
;; constraints in `S.c-store` and display a dict of constraint reifications for
;; each reified lvar.
;; E.g. `[_.0 {"=/=" [_.0 a] "absento" []}]`

(defn reify-s [v s]
  "Reify a substitution.

TODO: This could use some form of t.c.o.
"
  (let [v (walk v s)]
    (cond
      [(var? v) (let [n (.format "_.{}" (len s))]
                  (if (instance? Mapping s)
                      (.set s v n)
                      (cons (cons v n) s)))]
      [(cons? v)
       (reify-s (cdr v)
                (reify-s (car v) s))]
      [True s])))
