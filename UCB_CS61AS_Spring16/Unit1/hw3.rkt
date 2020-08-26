#lang racket
(require berkeley)
(provide (all-defined-out))

#|
## Resources and Computing
Two dimensions influencing the efficient of the program:

* Space: e.g. The call stack is infinite and can occur stack overflow
	when the incomplete computations (calls) are too large.
* Time
|#

; **Recursive** version of factorial
; For large n, stack overflow will occur.
(define (factorial n)
  (if (<= n 0) 1 (* n (factorial (- n 1)))))

; **Iterative** version of factorial
; This version instead carried the **incomplete result to arguments**, therefore
; we need a helper procedure.
; i.e., we will store only the last result instead of the whole
; history calls.
; The key of space efficient in this case is that the product
; operation is called inside the iter and **performed before the recursive call**.
(define (factorial-iteration n)
  (define (iter prev-result n max-n)
    (if (<= n max-n)
      (iter (* n prev-result) (+ n 1) max-n)
      prev-result))
  (iter 1 1 n))

; Tail Call Elimination
; The Racket interpreter implements **Tail Call Elimination/Optimization** so that
; the iterative version is more space efficient than recursive version.

; Exercises
; 1. Q: Iterative factorial keeps track of three things in iter. What were those things? Could we rewrite factorial yet again in order to only keep track of two things?
; A: (1) incomplete result, current count, maximum count
;    (2) yep, refer to fact-iterative-two
; 2. Q: If we use an iterative version, will we ever run out of space calling factorial?
; A: Iterative version will never OOM in tail call optimized language,
;    bacause the procedure keeps only three arguments in anytime.

(define (fact-iterative-two n)
  (define (iter incomplete-result curr)
    (if (> curr 1)
      (iter (* incomplete-result curr) (- curr 1))
      incomplete-result))
  (iter 1 n))

; TODO: refactor to tail recursive version
(define (all-equal? list)
  (if (or (empty? list)
	  (empty? (bf list))
	  (empty? (bf (bf list))))
    #t
    (and (equal? (first list) (first (bf list)))
	 (all-equal? (bf list)))))
; test
(all-equal? (se (factorial 100) (fact-iterative-two 100) (factorial-iteration 100)))
