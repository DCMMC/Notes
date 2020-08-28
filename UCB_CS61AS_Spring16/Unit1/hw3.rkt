#lang racket
(require berkeley)
(require profile)
(require math/base)
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

; **Tail Call Elimination**
; The Racket interpreter implements **Tail Call Elimination/Optimization** so that
; the iterative version is more space efficient than recursive version.
; However, some unsupported languages such as JS, Python store function arguments on the call stack
; so that tail call optimization is trivial and hard to implement.

; Exercises
; 1. Q: Iterative factorial keeps track of three things in iter. What were those things? Could we rewrite factorial yet again in order to only keep track of two things?
; A: (1) incomplete result, current count, maximum count
;    (2) yep, refer to fact-iterative-two
; 2. Q: If we use an iterative version, will we ever run out of space calling factorial?
; A: Iterative version will never OOM in tail call optimized language,
;    bacause the procedure keeps only three arguments in anytime.

; optional argument
(define (fact-iterative-two n [incomplete-result 1])
  (if (> n 1)
    (fact-iterative-two (- n 1) (* incomplete-result n))
    incomplete-result))

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

(time (void (< 0 (factorial 20000))))
(time (void (< 0 (factorial-iteration 20000))))
(time (void (< 0 (fact-iterative-two 20000))))

; Time complexity
; omitted

; Exercises
(define (bar n)
  (if (zero? (remainder n 7))
    'Bzzst
    (bar (- n 1))))
; Time: O(n % 7)

(define (insert-sort s)
  (if (empty? s)
    '()
    (insert (insert-sort (bf s)) (first s))))

(define (insert sorted-sofar n)
  (if (empty? sorted-sofar)
    (se n)
    (if (< n (first sorted-sofar))
      (se n sorted-sofar)
      (se (first sorted-sofar)
	  (insert (bf sorted-sofar)
		  n)))))
; Time: O(n^2)

; **Tree recursive**
; exponential time

(define (count-stairs n)
  (cond [(= n 1) 1]
	[(= n 1) 2]
	[else (+ (count-stairs (- n 1))
		 (count-stairs (- n 2)))]))

; Exercises of HW3

; (define (even? n)
;   (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

; Iterative version of fast-expt
; **key**: use a helper argumenti *a* that is used to store incomplete result
(define (fast-expt-iter b n (a 1))
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter (square b) (/ n 2) a))
	(else (fast-expt-iter b (- n 1) (* a b)))))

; SICP 1.35
; **Fixed point** x of a function: f(x) = x
; One method to find fixed point: from an inital guess x0, repeat f(x0), f(f(x0)),
; f(f(f(x0))), ... until the value does not change very much.

; Note that fixed tolerance will failed for small and large values.
(define tolerance 0.00001)
(define (close-enough v1 v2)
  (< (abs (- v1 v2)) tolerance))
(define (fixed-point f init-guess)
  (define (try guess)
    ; Local binding: (let ([id val-expr] ...) body ...+) where id is a binded local variable
    ; for the body.
    (let ((next (f guess)))
      (if (close-enough guess next)
	guess
	(try (f guess)))))
  (try init-guess))

(define (square-root y)
  (fixed-point (lambda (x) (/ (+ x (/ y x)) 2)) 1))

; \phi^2 = \phi + 1, so \phi is fixed point of f(x) = 1 + 1 / x
(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))
(printf "phi=~s~n" (real->decimal-string phi 4))

; k-term finite continued fraction
; recursive version
(define (cont-frac n d k [i 1])
  (if (< i k)
    (/ (n i) (+ (d i) (cont-frac n d k (+ i 1))))
    (/ (n i) (d i))))
(close-enough (cont-frac (lambda (i) 1.0)
			 (lambda (i) 1.0)
			 100)
	      (/ 2 (+ 1 (square-root 5))))

; iterative version
(define (cont-frac-iter n d k [res 0])
  (if (<= k 1)
    (/ (n k) (+ (d k) res))
    (cont-frac-iter n d (- k 1)
		    (/ (n k) (+ (d k) res)))))
(close-enough (cont-frac-iter (lambda (i) 1.0)
			      (lambda (i) 1.0)
			      100)
	      (/ 2 (+ 1 (square-root 5))))

(close-enough (cont-frac-iter
		(lambda (i) 1.0)
		(lambda (i)
		  (if (equal? 0
			      (remainder (- i 2) 3))
		    (/ (* 2 (+ i 1)) 3)
		    1))
		100)
	      (- euler.0 2))

(define (e n)
  (+ 2 (cont-frac-iter
	 (lambda (i) 1.0)
	 (lambda (i)
	   (if (equal? 0
		       (remainder (- i 2) 3))
	     (/ (* 2 (+ i 1)) 3)
	     1))
	 n)))

; A perfect number is defined as a number equal to the sum of all
; its factors less than itself.
(define (next-perf n)
  (define (sum-of-factors num [curr 1] [sum 0])
    (if (equal? num 0) 1
      (if (equal? num curr) sum
	(if (zero? (remainder num curr))
	  (sum-of-factors num (+ 1 curr) (+ sum curr))
	  (sum-of-factors num (+ 1 curr) sum)))))
  (define (iter num)
    (if (equal? num (sum-of-factors num))
      num
      (iter (+ num 1))))
  (iter n))
(next-perf 29)


(define (count-change amount)
  (cc amount `(50 25 10 5 1)))

(define (cc amount kinds-of-coins)
  ; The two base cases cannot interchange, bacause when amount = 1 the kinds-of-coin
  ; is usually empty.
  (cond [(= amount 0) 1]
	[(or (< amount 0) (empty? kinds-of-coins)) 0]
	[else (+ (cc amount
		     (bf kinds-of-coins))
		 (cc (- amount
			(first kinds-of-coins))
		     kinds-of-coins))] ))


(define (invariant-expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
	       (- counter 1)
	       (* b product))))

; b^n = product \times b^{counter}
