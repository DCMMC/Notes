#lang racket

(require (rename-in graphics/turtles
		    (split turtle-split)))

(provide (all-defined-out))

;; Code for integrating Racket's turtle graphics library

(define turtle-x -1)
(define turtle-y -1)
(define turtle-pen-down #f)

(define (cs)
  (turtles #t)
  (clear)
  (set! turtle-x (/ turtle-window-size 2))
  (set! turtle-y (/ turtle-window-size 2))
  (set! turtle-pen-down #f))

(define (penup)
  (set! turtle-pen-down #f))

(define (pendown)
  (set! turtle-pen-down #t))

(define (setxy x y)
  (let ((relative-x (- x turtle-x))
	(relative-y (* -1 (- y turtle-y))))
    (begin (if turtle-pen-down
	     (draw-offset relative-x relative-y)
	     (move-offset relative-x relative-y))
	   (set! turtle-x x)
	   (set! turtle-y y))))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) turtle-window-size) (/ turtle-window-size 2))
	 (- (* (ycor-vect v1) turtle-window-size) (/ turtle-window-size 2)))
  (pendown)
  (setxy (- (* (xcor-vect v2) turtle-window-size) (/ turtle-window-size 2))
	 (- (* (ycor-vect v2) turtle-window-size) (/ turtle-window-size 2))))

(define (export filename)
  (save-turtle-bitmap (string->path filename) 'png))

;; Code for the picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
			    (edge1-frame frame))
		(scale-vect (ycor-vect v)
			    (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
	(draw-line
	  ((frame-coord-map frame) (start-segment segment))
	  ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

; use the relative positions (origin, corner1, corner2) related to the
; old frame to generate the positions of new frame.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	  (make-frame new-origin
		      (sub-vect (m corner1) new-origin)
		      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	    (transform-painter painter1
			       (make-vect 0.0 0.0)
			       split-point
			       (make-vect 0.0 1.0)))
	  (paint-right
	    (transform-painter painter2
			       split-point
			       (make-vect 1.0 0.0)
			       (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;; End of picture language code


;;
;; Your code goes below
;;

;; Exercise 1

(define (up-split painter n)
  ; (error "not yet implemented")
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;; Exercise 2

(define (split major minor)
  ; (error "not yet implemented")
  (define (fn painter n)
    (if (= n 0)
      painter
      (let ((sm (fn painter (- n 1))))
	(major painter (minor sm sm)))))
  fn)

;; Exercise 3

(define (make-vect major minor)
  ; (void "not yet implemented")
  (lambda (f) (f major minor)))

(define (xcor-vect v)
  ; "not yet implemented"
  (v (lambda (x y) x)))

(define (ycor-vect v)
  ; "not yet implemented"
  (v (lambda (x y) y)))

(define (add-vect v1 v2)
  ; (error "not yet implemented")
  (make-vect (+ (xcor-vect v1)
		(xcor-vect v2))
	     (+ (ycor-vect v1)
		(ycor-vect v2))))

(define (sub-vect v1 v2)
  ; (error "not yet implemented")
  (make-vect (- (xcor-vect v1)
		(xcor-vect v2))
	     (- (ycor-vect v1)
		(ycor-vect v2))))

(define (scale-vect s v)
  ; (error "not yet implemented")
  (v (lambda (x y)
       (make-vect (* s x) (* s y)))))

;; Execise 4

; First definition of make-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame
  ; "not yet implemented"
  (lambda (f) (car f)))

(define edge1-frame
  ; "not yet implemented"
  (lambda (f) (cadr f)))

(define edge2-frame
  ; "not yet implemented"
  (lambda (f) (caddr f)))

; Second definition of make-frame

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-2
  ; "not yet implemented"
  (lambda (f) (car f)))

(define edge1-frame-2
  ; "not yet implemented"
  (lambda (f) (cadr f)))

(define edge2-frame-2
  ; "not yet implemented"
  (lambda (f) (cddr f)))

;; Exercise 5

(define make-segment
  ; "not yet implemented"
  (lambda (start end)
    (lambda (f) (f start end))))

(define start-segment
  ; "not yet implemented"
  (lambda (s)
    (s (lambda (start end) start))))

(define end-segment
  ; "not yet implemented"
  (lambda (s)
    (s (lambda (start end) end))))

;; Exercise 6

(define outline-painter
  ; "not yet implemented"
  (lambda (frame)
    ((segments->painter
       (list (make-segment
	       (make-vect 0 0)
	       (make-vect 0 1))
	     (make-segment
	       (make-vect 0 0)
	       (make-vect 1 0))
	     (make-segment
	       (make-vect 1 0)
	       (make-vect 1 1))
	     (make-segment
	       (make-vect 0 1)
	       (make-vect 1 1))))
     frame)))

(define x-painter
  ; "not yet implemented"
  (lambda (frame)
    ((segments->painter
       (list (make-segment
	       (make-vect 0 0)
	       (make-vect 1 1))
	     (make-segment
	       (make-vect 1 0)
	       (make-vect 0 1))))
     frame)))

(define diamond-painter
  ; "not yet implemented"
  (lambda (frame)
    ((segments->painter
       (list (make-segment
	       (make-vect 0.5 0)
	       (make-vect 0 0.5))
	     (make-segment
	       (make-vect 0.5 0)
	       (make-vect 1 0.5))
	     (make-segment
	       (make-vect 1 0.5)
	       (make-vect 0.5 1))
	     (make-segment
	       (make-vect 0.5 1)
	       (make-vect 0 0.5))))
     frame)))

(define (wave-painter frame)
  ; "not yet implemented"
  (define (make-point point)
    (make-vect (car point) (cadr point)))
  (define (concat-seg points [res '()])
    (if (< (length points) 2)
      res
      (concat-seg
	(cdr points)
	(append res
		(list (make-segment
			(make-point (car points))
			(make-point (cadr points))))))))
  ((segments->painter
     (append
       (concat-seg
	 (list
	   (list 0 0.1512)
	   (list 0.4012 0.4537)
	   (list 0.2587 0)))
       (concat-seg
	 (list
	   (list 0.4 0)
	   (list 0.5 0.3)
	   (list 0.6 0)))
       (concat-seg
	 (list
	   (list 0.7512 0)
	   (list 0.6525 0.5)
	   (list 0.6987 0.6)
	   (list 0.85 0.4)
	   (list 1 0.6512)))
       (concat-seg
	 (list
	   (list 1 0.8525)
	   (list 0.85 0.6)
	   (list 0.7037 0.6537)
	   (list 0.6025 0.6537)
	   (list 0.6537 0.8537)
	   (list 0.6 1)))
       (concat-seg
	 (list
	   (list 0.4 1)
	   (list 0.35 0.8525)
	   (list 0.4 0.6537)
	   (list 0.2537 0.655)
	   (list 0 0.3575)))))
   frame))

;; Exercise 7
; **Key**: use the same painter but use different frame

(define (flip-horiz painter)
  ; (error "not yet implemented")
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

(define (rotate180 painter)
  ; (error "not yet implemented")
  (transform-painter
    painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)))

(define (rotate270 painter)
  ; (error "not yet implemented")
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))

;; Exercise 8

(define (below painter1 painter2)
  ; (error "not yet implemented")
  (lambda (frame)
    ((transform-painter
       painter1
       (make-vect 0 0)
       (make-vect 1 0)
       (make-vect 0 0.5))
     frame)
    ((transform-painter
       painter2
       (make-vect 0 0.5)
       (make-vect 1 0.5)
       (make-vect 0 1))
     frame)))

(define (below-2 painter1 painter2)
  ; (error "not yet implemented")
  (rotate90
    (beside
      (rotate270 painter1)
      (rotate270 painter2))))


;; Exercise 9

; Modify wave-painter above (Exercise 6)

; Modify me!
(define (corner-split-2 painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left up)
	    (bottom-right right)
	    (corner (corner-split-2 painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

; Modify me!
(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split-2 painter n))))

; **Key**: Learn from this project:
; 1. **data abstraction** (e.g. vect, segment, frame, painter) with its
; constructors and selectors.
; 2. **stratified design**: complex system structured with a sequence of
; levels. The parts constructed at each level are used as primitives at the
; next level. Stratified design makes progrmas **robust**: small changes
; in a specification will require correspondingly small changes in the program.

;; End of project
;; Don't touch anything below this

(define full-frame
  (make-frame (make-vect 0.5 0.5)
	      (make-vect 1 0)
	      (make-vect 0 1)))
