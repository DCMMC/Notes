#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?
DCMMC

2) What is your major?
CS

3) Are you a returning student? (i.e. Did you take 61AS last semester?)
Nope.

4) What made you to take 61AS?
Myself.

5) Tell us interesting things about yourself.
None.

|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


#|
DCMMC Notes:
(define ([name of procedure] [variables]) [body of procedure])
|#

;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

#|
DCMMC Notes:
## Words and sentences
Word: `'word` or `'(word)`
Sentence: `'(word1 word2)`
procedure `first` return the first letter of that word or the first word of that sentence
precedure `butfirst` or `bf` takes a word/sentence and returns everything but the first letter/word

## Special forms
procedures with special evaluation rules (e.g. lazy evaluation instead of greedy avaluation in `if`))
### `if`: (if evl_cond evl_when_true evl_when_false)
e.g.
(if (= 5 (+ 2 3))
    'yay!
    (/ 1 0))

### `cond`: 
(cond (cond1 evl1)
      (cond2 evl2)
      ...
      (else evl_default))

### `and`:
(and arg1 ... arg_n)

> check whether all of its args are true

#t and #f indicate true and false

> `and` will **early stop** as soon as it can return false

Like `C`, anything not `#f` is `#t`, if `and` is `true`, it will return the value of its last arg

### `or`:

Like `and`, if `or` is `true`, it will return the last arg
|#

;; Exercise 2a - Define can-drive
(define (can-drive age) (if (< age 16) '(Not yet) '(Good to go)))


;; Exercise 2b - Define fizzbuzz
(define (devisible big small)
  (= (remainder big small) 0))
(define (fizzbuzz num)
  (if (number? num)
      (cond ((devisible num 15) 'fizzbuzz)
	    ((devisible num 3) 'fizz)
	    ((devisible num 5) 'buzz)
	    (else num))
      num))


;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if

#|
DCMMC Notes:
(define (infinite-loop) (infinite-loop))

(if (= 3 6)
  (infinite-loop)
  (/ 4 2))

So we need use macro to simulate `if`, because mcaro is evaluated before all functions.

to binds a macro to match multiple patternsa:
(define-syntax id
  (syntax-rules (literal-id ...)
    [pattern template]
    ...))
|#

#|
Your answer here
|#

;; DCMMC: this is wrong because all the args will greedy evaluated before the `if` is executed

(define (wrong-if test then-case else-case)
  (if test
    then-case
    else-case))

(define-syntax new-if
  (syntax-rules ()
    [(new-if test then-case else-case) ;; pattern that receives 3 args, `new-if` can write as `_` for convenience
     (cond (test then-case) ;; use cond to replace the origin macro, `if` is also a correct resolution
	   (else else-case))]))

(define (throw-exception) (/ 1 0))

(if (= 3 6)
  (throw-exception)
  (/ 4 2))

(new-if (= 3 6)
	(throw-exception)
	(/ 4 2))

#|
(wrong-if (= 3 6)
	  (throw-exception)
	  (/ 4 2))
|#

;;~
