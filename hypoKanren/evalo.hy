"Operators for a relational interpreter.

Adapted from https://tca.github.io/veneer/examples/editor.

"
(import [hypoKanren.cons [cons]])
(import [hypoKanren.goals [*]])
(import [hypoKanren.util [null?]])
(import [hypoKanren.cutil [*]])

(require [hypoKanren.goals [*]])
(require [hypoKanren.cutil [*]])


(make-constraint-system [=/= absento symbolo not-pairo]
                        valid-=/=
                        valid-absento
                        valid-symbolo
                        valid-not-pairo)

(defn lookupo [x env out]
    (fresh [y val env^]
      (== `(cons ~y ~val ~env^) env)
      (symbolo x)
      (symbolo y)
      (conde
        [[(== x y) (== val out)]
         [(=/= x y) (lookupo x env^ out)]])))

(defn unboundo [x env]
    (fresh []
      (symbolo x)
      (conde
        [[(== [] env)]
         [(fresh (y v env^)
                 (== `(cons ~y ~v ~env^) env)
                 (=/= x y)
                 (unboundo x env^))]])))

(defn eval-expo [expr env out]
    (fresh []
      (conde
        [[(symbolo expr) ;; variable
          (lookupo expr env out)]
         [(== `(quote ~out) expr)
          (absento 'closure out)
          (unboundo 'quote env)]
         [(fresh [x body] ;; abstraction
                 ;; TODO: Cover more Hy expression types (e.g. `defn`, `defmacro`).
                 (== `(fn [~x] ~body) expr)
                 (== `(closure ~x ~body ~env) out)
                 (symbolo x)
                 (unboundo 'fn env))]
         [(fresh [expr*]
                 (== `(cons list ~expr*) expr)
                 (unboundo 'list env)
                 (eval-exp*o expr* env out))]
         [(fresh [e1 e2 val x body env^] ;; application
                 (== `(~e1 ~e2) expr)
                 (eval-expo e1 env `(closure ~x ~body ~env^))
                 (eval-expo e2 env val)
                 (eval-expo body `(cons ~x ~val ~env^) out))]
         [(fresh [e1 e2 v1 v2]
                 ;; TODO: Support other forms of concatenation (e.g.
                 ;; `(+ [] [])`)
                 (== `(cons ~e1 ~e2) expr)
                 ;; TODO: Doesn't this seem redundant?  Also, we accept
                 ;; varargs in `cons`, so we should account for that.
                 (== `(cons ~v1 ~v2) out)
                 (unboundo 'cons env)
                 (eval-expo e1 env v1)
                 (eval-expo e2 env v2))]
         [(fresh [e v2]
                 (== `(car ~e) expr)
                 (unboundo 'car env)
                 (eval-expo e env `(cons ~out ~v2)))]
         [(fresh [e v1]
                 (== `(cdr ~e) expr)
                 (unboundo 'cdr env)
                 (eval-expo e env `(cons ~v1 ~out)))]
         [(fresh [e v]
                 ;; TODO: `null?` isn't a built-in; perhaps we should
                 ;; support `none?` and/or `empty?` instead?
                 (== `(null? ~e) expr)
                 (conde
                   [[(== [] v) (== True out)]
                    [(=/= [] v) (== False out)]])
                 (unboundo 'null? env)
                 (eval-expo e env v))]
         [(fresh [t c a b]
                 ;; TODO: Handle other conditional constructs
                 ;; (e.g. `case`, `when`, etc.)
                 (== `(if ~t ~c ~a) expr)
                 (unboundo 'if env)
                 (eval-expo t env b)
                 (conde
                   [[(== False b) (eval-expo a env out)]
                    [(=/= False b) (eval-expo c env out)]]))]])))

(defn eval-exp*o [expr* env out]
    (conde
      [[(== [] expr*) (== [] out)]
       [(fresh [a d res-a res-d]
               (== (cons a d) expr*)
               (== (cons res-a res-d) out)
               (eval-expo a env res-a)
               (eval-exp*o d env res-d))]]))
