"Relational operators for type inference

Adapted from https://tca.github.io/veneer/examples/editor.

"
(import [hypoKanren.cons [cons]])
(import [hypoKanren.goals [*]])
(import [hypoKanren.util [null?]])
(import [hypoKanren.cutil [*]])

(require [hypoKanren.goals [*]])
(require [hypoKanren.cutil [*]])


(make-constraint-system [=/= absento symbolo numerico]
                        valid-=/=
                        valid-absento
                        valid-symbolo
                        valid-numerico)

(defn !-o [Γ expr σ]
  "Relational operator for a typing relation.

More specifically, `(!-o Γ e σ)` represents the statement
\"Γ ⊢ e: σ, where e is a term of type σ in context Γ\".

"
  (conde
    [[(symbolo expr)
      (lookupo Γ expr σ)]
     [(numerico expr)
      (== 'int σ)]
     ;; FIXME: These generate degenerate answers.
     ;; [(== False expr)
     ;;  (== 'bool σ)]
     ;; [(== True expr)
     ;;  (== 'bool σ)]
     [(fresh [x e T1 T2]
             (== `(fn [~x] ~e) expr)
             (== `(~T1 → ~T2) σ)
             (symbolo x)
             (!-o `(cons (~x : ~T1) ~Γ) e T2))]
     [(fresh [f e e^ t-ignore]
             (== `(let ((~f ~e)) ~e^) expr)
             (symbolo f)
             (!-o `(cons (~f poly ~e ~Γ) ~Γ) e^ σ)
             (!-o Γ e t-ignore))]
     [(fresh [e1 e2 T]
             (== `(~e1 ~e2) expr)
             (!-o Γ e1 `(~T → ~σ))
             (!-o Γ e2 T))]
     [(fresh [e1 e2]
             (== `(+ ~e1 ~e2) expr)
             (== 'int σ)
             (!-o Γ e1 'int)
             (!-o Γ e2 'int))]
     [(fresh [e1 e2 T1 T2]
             (== `(cons ~e1 ~e2) expr)
             ;; TODO: What's `pair` about?
             (== `(pair ~T1 ~T2) σ)
             (!-o Γ e1 T1)
             (!-o Γ e2 T2))]
     [(fresh [e1 e2 e3]
             (== `(if ~e1 ~e2 ~e3) expr)
             (!-o Γ e1 'bool)
             (!-o Γ e2 σ)
             (!-o Γ e3 σ))]]))

(defn lookupo [Γ x t]
  (fresh []
    (symbolo x)
    (conde
      [[(fresh [e Γ^ _]
               (== `(cons (~x poly ~e ~Γ^) ~_) Γ)
               (!-o Γ^ e t))]
       [(fresh [_]
               (== `(cons (~x : ~t) ~_) Γ))]
       [(fresh [y _ Γ^]
               (== `(cons ~y ~_ ~Γ^) Γ)
               (=/= x y)
               (symbolo y)
               (lookupo Γ^ x t))]])))
