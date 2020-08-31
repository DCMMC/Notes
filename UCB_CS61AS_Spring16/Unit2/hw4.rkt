#lang racket
(require berkeley)
(provide (all-defined-out))

(define (func-pair p) ((car p) (cdr p)))

; **Pair**: cons: (cons 1 2) => (1 . 2)
; first: car, second: cdr
; '() is empty list, (cons 2 (cons 1 '())) => (2 1)
; **List**, the above statement is equivalent to (list 2 1)
; In racket, every list is a pair whose cdr is another list.
; It is actually a single linked list.
; Useful procedures:
; 1. Procedure *append* must be called with two or more arguments,
; where the first argument is a list.
; 2. (list-ref list index) to get item
; 3. null? length, note that null is equivalent to '() (i.e., empty list)
; 4. map, filter, accumalate (foldl and foldr in racket)

; **Data abstraction** uses compound data and corresponding operates to strcuture
; the programs.
; **ADT (Abstract Data Type)** hides the detail implementations of the object and
; exposes the abstract interface to users to maniplulate the object.
; Going under the abstraction and making assumptions on how the data structure
; was implemented is called a **data abstraction violation (DAV)**.

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  ; (error "Not yet implemented")
  (cdr interval))

(define (lower-bound interval)
  ; (error "Not yet implemented")
  (car interval))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  ; (error "Not yet implemented")
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (>= (* (lower-bound y) (upper-bound y)) 0)
    (mul-interval x
		  (make-interval (/ 1 (upper-bound y))
				 (/ 1 (lower-bound y))))
    (raise-argument-error 'div-interval
			  "y spans zero"
			  y)))


;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  ; (error "Not yet implemented")
  (make-interval (- c (* c (/ tol 100)))
		 (+ c (* c (/ tol 100)))))
(define (percent interval)
  (/ (- (upper-bound interval) (center interval))
     (/ (center interval) 100)))

; SICP 2.17 - Define last-pair

(define (last-pair lst)
  ; (error "Not yet implemented")
  (cons (list-ref lst (- (length lst) 1)) '()))

; SICP 2.20 - Define same-parity

; define with **dotted-tail notation** is used to take arbitrary
; number of arguments.
(define (same-parity . lst)
  ; (error "Not yet implemented. Do not forget to edit the arguments of this procedure as well.")
  (define (iter lst res)
    (if (null? lst)
      res
      (if (equal? (odd? (car res))
		  (odd? (car lst)))
	(iter (cdr lst) (append res (cons (car lst) '())))
	(iter (cdr lst) res))))
  (if (null? lst)
    lst
    (iter (cdr lst) (cons (car lst) '()))))

; SICP 2.22 - Write your explanation in the comment block:

#|
Your explanation here
1. Because (square (car things)) will left-appended to answer.
2. Because (cons '() 1) produces ('() . 1) instead of (1)
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  ; (error "Not yet implemented")
  (if (list? lst)
    (if (null? lst)
      lst
      (cons (substitute (car lst) old new)
	    (substitute (cdr lst) old new)))
    (if (equal? lst old)
      new
      lst)))

; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  ; (error "Not yet implemented")
  (define (list-replace wd old-list new-list)
    (if (null? old-list)
      wd
      (if (equal? wd (car old-list))
	(car new-list)
	(list-replace wd (cdr old-list) (cdr new-list)))))
  (define (iter lst old new)
    (if (list? lst)
      (if (null? lst)
	lst
	(cons (iter (car lst) old new)
	      (iter (cdr lst) old new)))
      (list-replace lst old new)))
  (iter lst old new))

; Extra exercises
; Exercise 4
; Write the procedure cxr-function that takes as its argument a
; word starting with c, ending with r, and having a string of
; letters a and/or d in between, such as cdddadaadar. It should
; return the corresponding function.
(define (cxr-function name)
  (let ((seq (bf (bl name))))
    (define (get chr f)
      (if (equal? chr 'a)
	(lambda (x) (car (f x)))
	(lambda (x) (cdr (f x)))))
    (define (generator ops)
      (if (empty? ops)
	(lambda (x) x)
	(get (first ops) (generator (bf ops)))))
    (generator seq)))

(define (equal-function? f1 f2 x res)
  (and (equal? res (f1 x))
       (equal? res (f2 x))))

(printf "check cxr-function: ~s~n"
	(equal-function? (cxr-function 'caddr) caddr
			 '(1 2 3 4)
			 3))

; Church numerals
; sidenote (DCMMC): Church numerals are very likely to function composition.
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
; or:
; (define one (lambda (f) f))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (lambda+ m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(printf "check Church numerals: ~s~n"
	(equal-function? ((lambda+ one (lambda+ zero two)) add1)
			((lambda (f) (lambda (x) (f (f (f x))))) add1)
			1 4))

; reverse list
(define (my-reverse lst [res '()])
  (if (null? lst)
    res
    (my-reverse (cdr lst)
	     (append (cons (car lst) '()) res))))
(printf "check my-reverse: ~s~n"
	(equal? (my-reverse (list 1 4 9 16 25))
		(list 25 16 9 4 1)))
