"Relational operators for type inference

Adapted from https://tca.github.io/veneer/examples/editor.

"
(import [hypoKanren.cons [cons]])
(import [hypoKanren.goals [*]])
(import [hypoKanren.util [null?]])
(import [hypoKanren.cutil [*]])

(require [hypoKanren.goals [*]])
(require [hypoKanren.cutil [*]])


(make-constraint-system [=/= absento symbolo numerico booleano]
                        valid-=/=
                        valid-absento
                        valid-symbolo
                        valid-numerico
                        valid-booleano)

(defn !-o [Γ expr σ]
  "Relational operator for a typing relation.

More specifically, `(!-o Γ e σ)` represents the statement
\"Γ ⊢ e: σ, where e is a term of type σ in context Γ\".
"
  (conde
    [;; Primitive types
     [(symbolo expr)
      ;; Find the type associated with this symbol.
      (lookupo Γ expr σ)]
     [(numerico expr)
      (== 'int σ)]
     [(booleano expr)
      (== 'bool σ)]
     ;; Basic forms
     [(fresh [x e T1 T2]
             ;; Function form
             (== `(fn [~x] ~e) expr)
             (== `(~T1 → ~T2) σ)
             (symbolo x)
             (!-o (cons `(~x :: ~T1) Γ) e T2))]
     [(fresh [f e e-let t]
             ;; `let` construct
             (== `(let [~f ~e] ~e-let) expr)
             (symbolo f)
             ;; Create the lexical scope induced by a `let` form.
             (!-o (cons `(~f poly ~e ~Γ) Γ) e-let σ)
             (!-o Γ e t))]
     [(fresh [rator rand T]
             ;; [unary] Function application
             ;; TODO: Use `(cons 'a '())`
             ;; (conso rator '()) expr
             (== `(~rator ~rand) expr)
             (!-o Γ rator `(~T → ~σ))
             (!-o Γ rand T))]
     [(fresh [e1 e2]
             ;; Addition function
             (== `(+ ~e1 ~e2) expr)
             (== 'int σ)
             (!-o Γ e1 'int)
             (!-o Γ e2 'int))]
     [(fresh [e1 e2 T1 T2]
             ;; Cons function
             (== `(cons ~e1 ~e2) expr)
             (== `(pair ~T1 ~T2) σ)
             (!-o Γ e1 T1)
             (!-o Γ e2 T2))]
     [(fresh [e1 e2 e3]
             ;; Conditional form
             (== `(if ~e1 ~e2 ~e3) expr)
             (!-o Γ e1 'bool)
             (!-o Γ e2 σ)
             (!-o Γ e3 σ))]]))

;; (defn !- [env exp t]
;;   (conde
;;     [[(symbolo exp) (lookupo exp env t)]
;;      [(fresh [x e t-x t-e]
;;              (== `(fn [~x] ~e) exp)
;;              (symbolo x)
;;              (not-in-envo 'fn env)
;;              (== `(-> ~t-x ~t-e) t)
;;              (!- e (cons `(cons ~x ~t-x) env) t-e))]
;;      [(fresh [rator rand t-x]
;;              (== `(~rator ~rand) exp)
;;              (!- rator env `(-> ~t-x ~t))
;;              (!- rand env t-x))]]))

;; (defn lookupo [env x t]
;;   (fresh [r y v]
;;          (== (cons `(cons ~y ~v) r) env)
;;          (conde
;;            [[(== y x) (== v t)]
;;             [(=/= y x) (lookupo x r t)]])))

(defn lookupo [Γ x t]
  "Relation stating `x : t` (i.e. `x` has type `t`) in environment Γ."
  ;; (do (import pdb) (pdb.set-trace))
  (fresh []
    (symbolo x)
    (conde
      ;; First element in environment list matches our term and states a
      ;; polymorphic relationship to a type in a sub-environment (e.g. lexical
      ;; env produced by `let`).
      [[(fresh [e Γ^ r]
               ;; FIXME: Something's wrong with unification here?
               (== (cons `(~x poly ~e ~Γ^) r) Γ)
               (!-o Γ^ e t))]
       ;; First element in environment list matches our term.
       [(fresh [r]
               (== (cons `(~x :: ~t) r) Γ))]
       ;; Look elsewhere in the environment list.
       [(fresh [y r Γ^]
               (== (cons (cons y r) Γ^) Γ)
               (=/= x y)
               (symbolo y)
               (lookupo Γ^ x t))]])))

(defn not-in-envo [x env]
  (conde
    [[(== [] env)]
     [(fresh [y v r]
             (== (cons (cons y v) r) env)
             (=/= y x)
             (not-in-envo x r))]]))
