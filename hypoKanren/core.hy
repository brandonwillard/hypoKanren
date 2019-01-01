"microKanren core objects and goals.

"
(import functools)
(import [multipledispatch [dispatch]])
(import [pyrsistent [pmap pset]])
(import [hypoKanren.cons [cons cons?]])
(import [hypoKanren.stream [unit mplus bind mzero]])
(import [hypoKanren.unify [*]])

(require [hy.contrib.walk [let]])


(defclass KanrenState [object]
  "A microKanren state object.

TODO: `c-store` is currently unused.  We need to decide how constraints and
substitutions should be handled (e.g. only track one map of constraints to
their rands--including `==`), track `==` substitutions in `subs` and constraint
rands in `c-store`, etc.)
"
  (setv __slots__ ["subs" "lvar_count" "c_store" "domains"])
  (defn --init-- [self &optional [subs None] [lvar-count 0] [c-store None] [domains None]]
    (setv self.subs (if (none? subs) (LVarDAG) subs)
          self.lvar-count lvar-count
          self.c-store (if (none? c-store) (pmap) c-store)
          self.domains domains))
  (defn new-subs [self s]
    "Create a copy of the current state with new `subs`."
    (KanrenState s self.lvar-count self.c-store self.domains))
  (defn new-lvar-count [self c]
    "Create a copy of the current state with new `lvar-count`."
    (KanrenState self.subs c self.c-store self.domains))
  (defn new-c-store [self c-s]
    "Create a copy of the current state with new `c-store`."
    (KanrenState self.subs self.lvar-count c-s self.domains))
  (defn new-domains [self d]
    "Create a copy of the current state with new `domains`."
    (KanrenState self.subs self.lvar-count self.c-store d))
  (defn --repr-- [self]
    (.format "({} {} {} {} {})"
             (. self __class__ __name__)
             (repr self.subs)
             (repr self.lvar-count)
             (repr self.c-store)
             (repr self.domains)))
  (defn --hash-- [self]
    (hash (, self.subs self.lvar-count self.c-store self.domains)))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.subs other.subs)
         (= self.lvar-count other.lvar-count)
         (= self.c-store other.c-store)
         (= self.domains other.domains))))

#@((dispatch KanrenState object object)
   (defn ext-S [S c-key c-rands]
     "Add constraint operator rands/constrained terms to a given constraint in a
constraint store.

To illustrate, consider the representation of a dis-equality constraint like
`(=/= a b)`; the `c-key` is `=/=` and the `c-rands` are `(cons a b)`.

In the case of the unification \"constraint\", unification is actually performed


Parameters
==========
S: `KanrenState`
  The state object containing the constraint store (we use `S.subs`).
c-key: symbol
  A symbol representing the constraint in the constraint store (e.g. key in a
  dict).
c-rands: `cons` pair
  A `cons` of rands/arguments to the constraint represented by `c-key`.  These
  rands/arguments effectively represent actively constrained terms.
"
     (cond
       [(= c-key '==)
        (let [subs S.subs
              s-new (unify #* c-rands subs)]
          (unless (none? s-new)
            (S.new-subs s-new)))]
       [True
        (do
          (let [old-s (.get S.c-store c-key [])]
            (S.new-c-store
              (ext-s c-key
                     (cons c-rands old-s)
                     S.c-store))))])))

(defn state? [x]
  "Predicate for a Kanren state object."
  (instance? KanrenState x))

(defn make-state [&rest args]
  "Create an empty Kanren state object.

Just a pass-through for the class constructor.
"
  (let [subs (first args)
        subs (if (instance? LVarDAG subs)
                 subs
                 (LVarDAG subs))]
    (if (not (none? subs))
        (KanrenState subs #* (rest args) )
        (KanrenState))))

(defmacro λₘ [args &rest body]
  "The goal creation macro.  Use this to construct a goal.

The first argument must be the current state, and the body must evaluate to a
 state streams.

TODO: Should this macro even take an argument list?
"
  `(fn ~args ~@body))

(defn disj [g1 g2]
  "Goal disjunction: produces a goal that returns the appended state stream
from all successful goal arguments.

In other words, it behaves like logical disjunction/OR for goals."
  (λₘ [S]
      (mplus (g1 S) (g2 S))))

(defn conj [g1 g2]
  "Goal conjunction: produces a goal that returns the appended state stream in which
all both goals are necessarily successful.

In other words, it behaves like logical conjunction/AND for goals."
  (λₘ [S]
      (bind (g1 S) g2)))

;; Basic goals
(defn == [u v]
  "Create a goal that performs unification of this function's arguments.

Since a goal is, by definition, a function that takes a state argument (and
returns another state), and, since the aforementioned unification occurs in the body
of said goal, the unification produced here will necessarily be performed under
arbitrary states (themselves containing--or entirely consisting of--existing
substitutions)."
  (λₘ [S]
      (let [s (unify u v S.subs)]
        ;; XXX: Empty dicts are False-y.
        (if (none? s)
            ;; Update the lvar mappings in the state object.
            mzero
            (unit (.new-subs S s))))))

(defn call/fresh [f]
  "Create a goal from the evaluation of a lambda, `f`, given a logic variable as
its sole argument.

In other words, `f` is called with a fresh lvar.  Naturally, the body of the
lambda, `f`, must produce a goal.

Example
=======
  => (next (call/empty-state (call/fresh (fn [q] (== q 1)))))

"
  (λₘ [S]
      (let [c S.lvar-count]
        ((f (var c))
         ;; Update the lvar count in the state object.
         (.new-lvar-count S (inc c))))))

(defn call/empty-state [g]
  "Evaluate a goal with an empty state object."
  (g (make-state)))
