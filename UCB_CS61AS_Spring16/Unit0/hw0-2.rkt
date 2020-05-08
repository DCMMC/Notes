#lang racket

(require berkeley)
(provide (all-defined-out))

#|
There are many *primitie procedures* in Racket:
e.g. sqrt, max, +, *, -

Fromat of procedure:
(operator operand_1 ... operand_n)

Expressions are written as *prefix notation*.
Nested expression:
e.g. (* (max 2 3) (/ 8 4))

Compound Procedure:
e.g. (define (square x) (* x x))

## 1. quote
word and sentence notation ' is abbreviation for the function
*quote*.
'x is equal to (quote x)
'(a b) is equal to (quote a b)

> quote of sentence is more likely list instead of string

> quote is a *special forms* which does not *evaluate* its arguments.

## 2. word
used to concateante many word quote
e.g. (word 'a 'b) => 'ab

> empty word is represented as ""

## 3. sentence or se
create one sentence of all its arguments with words or sentences
e.g. (se 'foo '(foo bar) 'bar '()) => '(foo foo bar bar)

> empty sentence is denoted as '()

### 4. boolean related precedures
In Racket, #t and #f denote true and false

* Mathematical operators: <, >, <=, >=, =
* member? #t if the first arg (letter or word) is contained in second arg (word or sentence)
* even? #t if the number is even
* empty? #t if the argument's length is 0
* equal? same as = which is only for number but support any types
* false?
* number?
* word?
* sentence?
* boolean?

> ? in above is just a normal alphabet for precedure's name

> Anything that isnot false is true
e.g. (false? 0) => #f

logical operations:

and, or, not, nand (i.e. (not (and ...))), nor (i.e., (not (or ...)))
or returns the first argument that is not false
xor takes two arguments of any type and, if exactly one (no more or less) of its arguments is not #f, return that argument. Otherwise, return #f.

|#

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10

;2. Compound Expression (3 Atoms)
(+ 2 8)

;3. Compound Expression (4 Atoms)
(+ 2 2 6)

;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (+ 2 2) (+ 3 3))

;5. Any Other Kind Expression
(sqrt 100)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  ; (error "Not yet implemented")
  (word (first wd) (second wd))
)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  ; (error "Not yet implemeted")
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  ; (error "Not yet implemented")
  (word (first (first sent)) (first (second sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  ; (error "Not yet implemented")
  (and (>= num 13) (<= num 19))
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  ; (error "Not yet implemented")
  (se
    (if (member? (first wd) 'aeiou) 'an 'a)
    wd)
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  ; (error "Not yet implemented")
  (if (= 1 (count sent))
    sent
    (se (bl sent) 'and (last sent))
  )
)

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  ; (error "Not yet implemented")
  (se (second sent) (first sent) (bf (bf (bl sent))) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  ; (error "Not yet implemented")
  (cond
    ((equal? (second time) 'am)
     (if
       (= (first time) '12)
       0 (first time)
     ))
    ((equal? (second time) 'pm)
     (if
       (= (first time) '12)
       12 (+ 12 (first time))))
  )
)


(define (american-time time)
  ; your code here
  ; (error "Not yet implemented")
  (cond
    ((= time 0) '(12 am))
    ((= time 12) '(12 pm))
    ((< time 12) (se time 'am))
    ((> time 12) (se (- time 12) 'pm))
  )
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  ; (error "Not yet implemented")
  (cond
    ((< secs 60) (se secs 'seconds))
    ((< secs (* 60 60)) (se (/ secs 60.0) 'minutes))
    ((< secs (* 60 60 60)) (se (/ secs 60.0 60.0) 'hours))
    (else (se (/ secs 60.0 60 24) 'days))
  )
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

Explanation here.

The variable definition of superlative contains defined name: word

|#
