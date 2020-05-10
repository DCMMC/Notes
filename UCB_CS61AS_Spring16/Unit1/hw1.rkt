#lang racket

#|
function: for a given input, the outputs are always same
for a certain fucntion whenever it called.


However, procedures have not this restriction.
e.g. random

In Racket, all functions are procedures, but not all procedures are functions.

Evaluation method:

* Applicative Order: evaluating operator, evaluating the operands and then applying the operator
* Normal Order: not evaluate the operand until the value is needed.

e.g.
(define (double-first a b) (+ a a))

(double-first (+ 1 1) (+ 2 2))

For applicative order, the (+ 1 1) and (+ 2 2) will be evaluated
1 and 1 times.
For normal order, them will be evaluated 2 and 0 times.

### Internal definitions

We can define procedures and variables in the body of a procedure.
When a function defined inside another function, the one inside has access to variables and parameters of the outer function.
|#

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  ; Your code here
  ; (error "Not yet implemented")
  (define (if-in current result)
    (if (empty? result)
      #f (or (equal? current (first result))
	     (if-in current (bf result)))))
  (define (iter-add remain result)
    (if (empty? remain)
      result
      (if (if-in (last remain) result)
	 (iter-add (bl remain) result)
	 (iter-add (bl remain) (se result (last remain)))
      )))
  (iter-add sent '())
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  ; Your code here
  ; (error "Not yet implemented")
  (cond
    ((empty? sent) 0)
    ((equal? (first sent) wd)
     (+ 1 (count-word (bf sent) wd)))
    (else (count-word (bf sent) wd)))
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here

Infinite loop.
Because the operands of procedure will be lazy evaluated, i.e.,
applicative order.
|#

; Exercise 4 - Define squares

(define (squares sent)
  ; Your code here
  ; (error "Not yet implemented")
  (define (square x) (* x x))
  (define (squares-iter remain)
    (if (empty? remain)
      '()
      (se (square (first remain)) (squares-iter (bf remain)))))
  (squares-iter sent)
)

; Exercise 5 - Define switch

(define (switch sent)
  ; Your code here
  ; (error "Not yet implemented")
  (define (replace-word wd begin)
    (cond
      ((equal? wd 'I) 'you)
      ((equal? wd 'me) 'you)
      ((equal? wd 'you)
       (if begin 'I 'me))
      (else wd)))
  (define (switch-general remain)
    (if (empty? remain)
      '()
      (se (replace-word (first remain) #f)
	  (switch-general (bf remain)))))
  (if (empty? sent)
    '()
    (se (replace-word (first sent) #t)
	(switch-general (bf sent))))
)

; Exercise 6 - Define ordered?
(define (second sent)
  (first (bf sent)))
(define (ordered? sent)
  ; Your code here
  ; (error "Not yet implemented")
  (if (< (count sent) 2)
    #t
    (and (< (first sent) (second sent)) (ordered? (bf sent))))
)

; Exercise 7 - Define ends-e

(define (word-ends-e? wd)
  (or (equal? 'e (last wd)) (equal? 'E (last wd))))
(define (ends-e sent)
  ; Your code here
  ; (error "Not yet implemented")
  (cond
    ((empty? sent) '())
    ((word-ends-e? (first sent))
     (se (first sent) (ends-e (bf sent))))
    (else (ends-e (bf sent))))
)

; Exercise 8

#|

Your explanation here

Like many other languages, e.g. Lisp, Java, the boolean
expressions are special forms, a.k.a short circuiting.

The advantageous of treating logic expression as special form is:
if we already know the result, we can early stop evaluating the remain
operands to reduce unnecessary computations.

The advantage of the opposite is:
1. we can make use of the side effect of the evulation.
2. let compiler to have the freedom to reorder and prune expressions.

|#
