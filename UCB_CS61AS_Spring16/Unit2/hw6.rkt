#lang racket

(require berkeley)
(provide (all-defined-out))

; ## Tagged data
; Attach tags to data structures with same internal representation
; to distinguish them.
; When we want to apply an operator to the tagged data, the generic
; procedure will use cond to determine the underlying procedure
; according to the tag.
;
; Weakness of tagged data:
;
; * every data type must be identified and manually incorporated
; into every generic procedure so that it can call the underlying procedure
; accordingly.
; * must guarantee that no two underlying procedures in the entire system
; have the same name
;
; Conclusion: tagged data method for implementing generic interfaces does not
; scale.
;
; ## Data-directed programming
; We can store the underlying procedure as value (lambda) in the hash table.
; In this case, we will scale the generic interface. Because if we add new data
; type, we only need implement the underlying procedure and store it in hash table,
; and do not need modify the generic procedure itself.
;
; ## Racket-1 interpreter.
;
; Read-Eval-Print Loop (REPL):
; Every time you type a command, racket-1 parses and executes your input, returns the
; output, and then waits for another command.
;
; Define higher-order precedure with only lambda, **the Y-combinator trick**:
; example of map:
; ((lambda (f n)  ; this lambda is defining MAP
;     ((lambda (map) (map map f n))
;     (lambda (map f n)
;         (if (null? n)
;             '()
;             (cons (f (car n)) (map map f (cdr n))) )) )) ;end of lambda defining MAP
; first              ; the argument f for MAP
; '(the rain in spain)) ; the argument n for MAP
;
; Key idea: pass the lambda itself as its parameter
