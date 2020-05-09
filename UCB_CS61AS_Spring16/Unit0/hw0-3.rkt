#lang racket

#|
Recursion:

e.g.
(define (factorial n)
  (if (<= n 0)
    1 ; base case
    (* n (factorial (- n 1))) # recursive case
  )
)

(define (count-ums str)
  (if (empty? str)
      0
      (if (equal? 'um (first str))
          (+ 1 (count-ums (bf str)))
          (count-ums (bf str))
       )
   )
)

(equal? 3 (count-ums '(today um we are going to
                   um talk about the um
                   combining method)))

(equal? 0 (count-ums '()))


(define (countdown n)
  (if (<= n 0)
      'blastoff!
      (se n (countdown (- n 1)))
   )
)

(countdown 10)
(countdown 3)
(countdown 1)
(countdown 0)


## Common recursive patterns:

* every: iterate and return every element
* keep: choosing some of the elements and filtering out the others
* accumulate: which combines all of the elements of the argument into a single result
|#

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (floor-op x y)
  (quotient x y))

(define (remainder-op x y)
  (- x (* y (floor-op x y))))

(define (handle-plural n unit)
  (if (> n 1)
      (se n (word unit 's))
      (se n unit)
  )
)
;; toggle for handling plural
(define if-handle-plural #f)

(define (handle-plural-wrapper n unit)
  (if if-handle-plural
      (handle-plural n unit)
      (se n (word unit 's))
  )
)

(define (handle-unit n level unit postfix)
  (se (describe-time-helper
        (quotient n unit)
        (+ 1 level))
      (handle-plural-wrapper
        (remainder-op n unit) postfix)
  )
)

; level: 1: second, 2: minute, 3: hour, 4: day
(define (describe-time-helper n level)
  (cond
    ((equal? n 0)
     '())
    ((equal? level 1)
     (handle-unit n level 60 'second))
    ((equal? level 2)
     (handle-unit n level 60 'minute))
    ((equal? level 3)
     (handle-unit n level 24 'hour))
    ((equal? level 4)
     (handle-unit n level 366 'day))
    (else '())
  )
)

(define (describe-time secs)
  ; your code here
  (describe-time-helper secs 1)
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  ; your code here
 ; (error "Not yet implemented")
 (if (equal? wd (first sent))
   (bf sent)
   (se (first sent) (remove-once wd (bf sent))))
)

; Exercise 3 - Define differences
(define (second sent)
  (first (bf sent))
)
(define (differences nums)
  ;your code here
 ; (error "Not yet implemented")
 (if (< (count nums) 2)
   '()
   (se (- (second nums) (first nums))
       (differences (bf nums))))
)

; Exercise 4 - Define location
(define (location-impl small big)
 (if (equal? 0 (count big))
   1
   (if (equal? small (first big))
     1
     (+ 1 (location-impl small (bf big))))
 )
)
(define (if-not-found idx big)
  (if (> idx (count big))
    #f
    idx))
(define (location small big)
  ; your code here
 ; (error "Not yet implemented")
 (if-not-found (location-impl small big) big)
)

; Exercise 5 - Define initials
(define (initials sent)
  ; your code here
  ; (error "Not yet implemented")
  (if (equal? '() sent)
    '()
    (se (first (first sent)) (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd)
  ; your code here
 ; (error "Not yet implemented")
 (if (<= num 0)
   '()
   (se wd (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (base-grade grade)
  (cond
    ((equal? (first grade) 'A) 4)
    ((equal? (first grade) 'B) 3)
    ((equal? (first grade) 'C) 2)
    ((equal? (first grade) 'D) 1)
    ((equal? (first grade) 'E) 0)))
(define (grade-modifier grade)
  (cond
    ((equal? (bf grade) "") 0.0)
    ((equal? (bf grade) '+) 0.33)
    ((equal? (bf grade) '-) -0.33)))
(define (gpa-no-average grades)
  ; your code here
 ; (error "Not yet implemented")
 (if (<= (count grades) 0)
   0.0
   (+ (base-grade (first grades))
      (grade-modifier (first grades))
      (gpa-no-average (bf grades))))
)
(define (gpa grades)
  (/ (gpa-no-average grades) (count grades)))

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  ; your code here
 ; (error "Not yet implemented")
 (if (empty? sent)
   '()
   (if (number? (first sent))
     (se (copies (first sent) (second sent))
	 (repeat-words (bf (bf sent))))
     (se (first sent)
	 (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
(define (same-count? a b)
 (equal? (count a) (count b))
)
(define (same-shape-letter? sent1 sent2)
  (if (empty? sent1)
    #t
    (and (same-count? (first sent1) (first sent2))
	 (same-shape-letter? (bf sent1) (bf sent2)))))
(define (same-shape? sent1 sent2)
  ; your code here
 ; (error "Not yet implemented")
 (if (same-count? sent1 sent2)
   (same-shape-letter? sent1 sent2)
   #f)
)
