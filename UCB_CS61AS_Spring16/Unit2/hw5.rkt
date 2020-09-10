#lang racket

; **Notes**:
; letter-t trees: binary tree constructed from single linked list.
; capital-T tree: tree ADT without specific implementations
;
; usage of higher order functions as abstraction:
; enumerate (generate sequence of data), filter,
; map, accumulate.
;
; Nested map is used for multi-dimensional sequence.
;
; **Mutual recursive**: The pattern of A calling B and B calling A
; is called mutual recursion.
;
; Read, Eval, Print, and Loop (REPL)

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.

; 1. (1 2 3 4 5 6)
; 2. ((1 2 3) 4 5 6)
; 3. ((1 2 3) (4 5 6))

; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (list-ref mobile 0))

(define (right-branch mobile)
  (list-ref mobile 1))

(define (branch-length branch)
  (list-ref branch 0))

(define (branch-structure branch)
  (list-ref branch 1))

; b. Define total-weight.

(define (enumerate-mobile mobile (length #f))
  (let ((length (if (eq? length #t) 0 length)))
    (if (number? mobile)
      (cons (if length (* mobile length) mobile) nil)
      (let ((left (branch-structure (left-branch mobile)))
	    (right (branch-structure (right-branch mobile)))
	    (left-len (branch-length (left-branch mobile)))
	    (right-len (branch-length (right-branch mobile))))
	(append (enumerate-mobile left (if length (+ length left-len) length))
		(enumerate-mobile right (if length (+ length right-len) length)))))))

(define (my-accumulate proc init seq)
  (if (null? seq)
    init
    (proc (car seq)
	  (my-accumulate proc init (cdr seq)))))

(define (total-weight mobile (length #f))
  ; (error "Not yet implemented")
  (my-accumulate + 0 (enumerate-mobile mobile length)))

; c. Define balanced?

(define (balanced? b-mobile)
  ; (error "Not yet implemented")
  )

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

; A: need to redefine left-branch, right-branch, branch-length,
; branch-structure

;Exercise 3a - Define square-tree

(define (square-tree d-l)
  ; (error "Not yet implemented")
  (tree-map square d-l))

(define (enumerate-tree tree)
  (cond ((null? tree) tree)
	((not (list? tree)) (cons tree '()))
	(else (cons (enumerate-tree (car tree))
		    (enumerate-tree (cdr tree))))))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  ; (error "Not yet implemented")
  (cond
    ((null? tree) tree)
    ((list? tree)
     (cons (tree-map fn (car tree))
	   (tree-map fn (cdr tree))))
    (else (fn tree))))

;Exercise 4 -  Complete the definition of accumulate-n

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (foldr op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m_i) (dot-product m_i v)) m))

(define (transpose mat)
  (accumulate-n
    (lambda (x y) (cons x y))
    nil
    mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (v) (matrix-*-vector cols v))
      m)))

;Exercise 6 - Give the property that op should satisfy:

#|

Your property here

(fold-right / 1 (list 1 2 3)): 1.5
(fold-left / 1 (list 1 2 3)): 1.5
(fold-right list nil (list 1 2 3)): (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)): (3 ( 2( 1())))

Property: to get same result of foldr and foldl,
op must be symmetric.

|#

;Exercise 7 - Define equal?

; Note that eq? is different from equal?

(define (my-equal? l1 l2)
  ; (error "Not yet implemented")
  (cond ((and (list? l1) (list? l2) (> (* (length l1) (length l2)) 0))
	 (and (my-equal? (car l1) (car l2))
	      (my-equal? (cdr l1) (cdr l2))))
	(else (eq? l1 l2))))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append
	rest (map
	       (lambda (lst) (cons (car s) lst))
	       rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond
    ((list? exp) (calc-apply
		   (car exp)
		   (map calc-eval (cdr exp))))
    (else exp)))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	((eq? fn 'first) (first (car args)))
	((eq? fn 'last) (last (car args)))
	((eq? fn 'butfirst) (butfirst (car args)))
	((eq? fn 'butlast) (butlast (car args)))
	((eq? fn 'bf) (butfirst (car args)))
	((eq? fn 'bl) (butlast (car args)))
	((eq? fn 'word) (foldr word "" args))
	(else (error "Calc: bad operator:" fn))))

; Extra exercises:
; Huffman trees: ommited

; Regroup problem:
(define (regroup pattern)
  ; Helpers
  (define (skip-n n lst)
    (if (> n 0)
      (skip (- n 1) (cdr lst))
      lst))
  (define (get-n n lst)
    (car (skip-n (- n 1) lst)))
  ())
