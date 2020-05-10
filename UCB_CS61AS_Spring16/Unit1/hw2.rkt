#lang racket

(require berkeley)
(provide (all-defined-out))

#|
### lambda fucntion
a.k.a. anonymous function
General form:

(lambda (<param1> <param2> ... <paramn>) <body>)

### `let`

Use `let` to create local variables.
General form:

(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
      (<varn> <expn>))
  <body>)

which is equivalent to:

((lambda (<var1> <var2> ... <varn>) <body>)
  <exp1> <exp2> ... <expn> )

### Higher order function (HOF)
Functions that take a function as an argument and/or return
a function as its output.

### A example of applied HOFs: square root

First we define **fix-point** x for a function f such that f(x) = x.

One algorithm is start from an initial guess x, and we repeat apply
f until we find it.

```racket
(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
       (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))
```

Therefore to find the square root of x, we just need to find the
fix-point y of function (1/2)(y + x/y).

```racket
(define (average-damp f)
     (lambda (y) (* 0.5 (+ y (f y)))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
```

The use of above function other than y = x / y is a technique
the SICP authors call **average damping**, often aids the
convergence of fixed-point searches.

> Acutally, this is **Newton's method**.
> More methods to find square root can be seen in http://en.wikipedia.org/wiki/Methods_of_computing_square_roots.


### `iterate`

The general form of **iterative improvement**. That is, you start with a value and keep improving it until it is good enough.

```racket
(define (iterate start improve good-enough?)
   (if (good-enough? start)
       start
       (iterate (improve start) improve good-enough?)))
```

|#

; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  ; Your code here
  ; (error "Not yet implemented")
  (cond
    ((empty? sent)
     '())
    ((equal? (first sent) old-word)
     (se new-word (substitute (bf sent) old-word new-word)))
    (else (se (first sent)
	      (substitute (bf sent) old-word new-word)))))


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns:

((lambda (x) (+ x 3)) 7)
-> returns:

(define (make-adder num)
  (lambda (x) (+ x num)))
((make-adder 3) 7)
-> returns:

(define plus3 (make-adder 3))
(plus3 7)
-> returns:

(define (square x) (* x x))
(square 5)
-> returns:

(define square (lambda (x) (* x x)))
(square 5)
-> returns

(define (try f) (f 3 5))
(try +)
-> returns:

(try word)
-> returns:
|#


; Exercise 3
#|

Number of arguments g has:

0

Type of value returned by g:

lambda

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
(define f1 1)

; body must not be empty
(define (f2) 1)

(define (f3 x) x)

(define f4 (lambda () (lambda () 1)))

(define f5 (lambda () (lambda () (lambda (x) x))))

; Exercise 5 - Try out the expressions

(define (t f)
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns:
3

2. ((t (t add1)) 0) returns:
9

3. (((t t) add1) 0) returns:
27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns:
3

2. ((t (t s)) 0) returns:
9

3. (((t t) s) 0) returns:
27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  ; Your code here
  ; (error "Not yet implemented")
  (lambda (x)
    (equal? x wd))
)

; Exercise 8 - SICP exercises

; SICP 1.31a
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  ; Your code here
  ; (error "Not yet implemented")
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1)
)

(define (factorial n)
  (product (lambda (x) x) 1
	   (lambda (x) (+ 1 x))
	   n))

(define (estimate-pi)
  ; Your code here
  ; (error "Not yet implemented")
  ; approximation of pi:
  ; pi = 4 * 0.5 * b * (2/3)^2 * (3/4)^2 * ... * (b/(b+1))^2
  (define a 2)
  (define b 1000)
  (define (next x) (+ 2 x))
  (define (square x) (* x x))
  (define (term x) (square (/ x (+ 1 x))))
  (* 4 0.5 b
     (product term a next (- b 2))))

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  ; Your code here
  ; (error "Not yet implemented")
  (define (iter curr)
    (if (> curr b)
      null-value
      (combiner (term curr) (iter (next curr)))))
  (iter a)
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  ; Your code here
  ; (error "Note yet implemented")
  (my-accumulate + 0 term a next b)
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  ; Your code here
  ; (error "Note yet implemented")
  (my-accumulate * 1 term a next b)
)


; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  ; Your code here
  ; (error "Not yet implemented")
  (define (iter curr)
    (cond
      ((> curr b) null-value)
      ((pred curr)
        (combiner (term curr) (iter (next curr))))
      (else
	(iter (next curr)))))
  (iter a)
)

(define (sum-sq-prime a b)
  ; Your code here
  ; (error "Not yet implemented")
  (filtered-accumulate + 0 (lambda (x) (* x x)) a
		       (lambda (x) (+ 1 x))
		       b prime?)
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  ; Your code here
  ; (error "Not yet implemented")
  (filtered-accumulate * 1 (lambda (x) x)
		       1 (lambda (x) (+ 1 x))
		       n
		       (lambda (x) (rel-prime? x n))))

; SICP 1.40 - Define cubic

(define (cubic a b c)
  ; Your code here
  ; (error "Not yet implemented")
  (define (square x) (* x x))
  (lambda (x)
    (+ c (* b x) (* (square x)
		    (+ x a)))))

; SICP 1.41 - Define double
; (((double (double double)) inc) 5) = 21

(define (double proc)
  ; Your code here
  ; (error "Not yet implemented")
  (lambda (x)
    (proc (proc x)))
)

; SICP 1.43 - Define repeated

(define (my-compose f g)
  (lambda (x)
    (f (g x))))
(define (my-repeated proc n)
  ; Your code here
  ; (error "Not yet implemented")
  (if (= 1 n)
    (lambda (x) (proc x))
    (my-compose proc (my-repeated proc (- n 1))))
)

; Exercise 9 - Define my-every

(define (my-every proc sent)
  ; Your code here
  ; (error "Not yet implemented")
  (define (iter remain)
    (if (empty? remain)
      '()
      (se (proc (first remain)) (iter (bf remain)))))
  (iter sent)
)

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns:
'(pp uu rr pp ll ee)

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns:
'(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns:
'(781 5 909)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns:
'(ooeee)

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:
""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:
Invalid arg for member?

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns:
'(purple)
|#
