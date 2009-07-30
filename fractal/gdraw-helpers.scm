;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NAME:        gdraw-helpers.scm
;;
;; DESCRIPTION: Supplementary graphics routines
;;              for gdraw.scm
;; 
;; AUTHOR:      Dan Garcia  -  University of California, Berkeley

;;              Copyright (C) 2008. All rights reserved.
;;
;; DATE:        2008-11-15
;;
;; UPDATE HIST:  
;;
;; 2008-11-15 v1.0  : Release 1.0
;; 2009-04-20 v2.0  : Added polar abilities (coord-Pp, sindeg, cosdeg, *Vp)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; CONSTANTS (to make our life easier)
;;;;;;;;;;;;

;; Window borders

(define *L*  0)
(define *R*  600)
(define *D*  0)    ;; This assumes (0,0) in SW

(define *U*  600)  ;; and the existence of y-flip

;; Numerical constants we'll use often (stk doesn't like fractions)

(define *PI* 3.14159265358979) 

(define -1/2 (/ -1 2))
(define  1/2 (/  1 2))


(define -2/3 (/ -2 3))
(define -1/3 (/ -1 3))
(define  1/3 (/  1 3))
(define  2/3 (/  2 3))

(define -3/4 (/ -3 4))
(define -1/4 (/ -1 4))
(define  1/4 (/  1 4))
(define  3/4 (/  3 4))

(define -4/5 (/ -4 5))

(define -3/5 (/ -3 5))
(define -2/5 (/ -2 5))
(define -1/5 (/ -1 5))
(define  1/5 (/  1 5))
(define  2/5 (/  2 5))
(define  3/5 (/  3 5))
(define  4/5 (/  4 5))

(define -5/6 (/ -5 6))
(define -1/6 (/ -1 6))

(define  1/6 (/  1 6))
(define  5/6 (/  5 6))

(define -7/8 (/ -7 8))
(define -5/8 (/ -5 8))
(define -3/8 (/ -3 8))
(define -1/8 (/ -1 8))
(define  1/8 (/  1 8))
(define  3/8 (/  3 8))
(define  5/8 (/  5 8))

(define  7/8 (/  7 8))

(define *sqrt3*   (sqrt 3))
(define *sqrt3/2* (/ *sqrt3* 2))
(define *-sqrt3/2* (- (/ *sqrt3* 2)))

;;;;;;;;;;
;; display
;; Redefine display to Compensate for the fact that stk doesn't flush on display

;;;;;;;;;;

(define display
   (let ((display display))
      (lambda (arg)
         (display arg)
         (flush))))

;;;;;;;;;;
;; y-flip
;;
;; Input:    y [num] ...assuming (0,0) is in NW corner

;; Requires: *U*, *D* be the (0,0) SW-corner screen limits 
;; Returns:  value of y assuming (0,0) is in SW corner
;;;;;;;;;;

(define (y-flip y)
  (+ (- *U* y) *D*))

;;;;;;;;;;
;; sinDeg
;;

;; Input:    thetaDeg [degrees] 
;; Returns:  sin(thetaDeg)
;;;;;;;;;;

(define (sinDeg thetaDeg)
  (sin (* thetaDeg (/ *PI* 180))))

;;;;;;;;;;
;; cosDeg
;;
;; Input:    thetaDeg [degrees] 

;; Returns:  sin(thetaDeg)
;;;;;;;;;;

(define (cosDeg thetaDeg)
  (cos (* thetaDeg (/ *PI* 180))))

;;;;;;;;;;
;; lerp
;;
;; Input:   x0 [num] "starting value"
;;          x1 [num] "ending value"

;;           t [num] "parameter"
;; Returns: Value (scalar) which is a linear combination of
;;          x0 and x1 using the standard formula:
;;          x(t) = x0 (1-t) + x1 (t)
;;          I.e., x(0) = x0, x(1) = x1, x(1/2)=(x0+x1)/2

;;
;; This is a VERY powerful function, allowing you to 
;; linearly interpolate between values x0 and x1.
;;
;; Example: (/ (+ x0 x1) 2) == (lerp x0 x1 1/2)
;;;;;;;;

(define (lerp x0 x1 t)
  (+ (* (- 1 t) x0)

     (*      t  x1)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATATYPE: POINT (P)
;; Constructors: make-P "make new Point P"
;; Selectors:    x-P    "get x-coord from P"
;;               y-P    "get y-coord from P"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-P x y) (list x y))
(define (x-P P)      (car  P))
(define (y-P P)      (cadr P))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATATYPE: VECTOR (V)

;; Constructors: make-V "make new Vector V"
;; Selectors:    x-V    "get x-coord from V"
;;               y-V    "get y-coord from V"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-V x y) (list x y))

(define (x-V V)      (car  V))
(define (y-V V)      (cadr V))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATATYPE: POLAR VECTOR (Vp)
;; Constructors: make-Vp  "make new Polar Vector Vp"
;; Selectors:    r-Vp     "get r-coord from Vp"

;;               theta-Vp "get theta-coord from Vp"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-Vp r theta) (list r theta))
(define (r-Vp     Vp)     (car  Vp))
(define (theta-Vp Vp)     (cadr Vp))


;;;;;;;;;;
;; draw-line-P
;;
;; Input:            P0, P1 [points]
;; Optional-inputs : options [gdraw inputs, as strings. E.g., "fill" "blue"]
;; SideEffects: Draws a line from P0 to P1

;;;;;;;;;;

(define (draw-line-P P0 P1 . options)
  (eval (append `(draw-line (x-P (quote ,P0)) (y-flip (y-P (quote ,P0)))
			    (x-P (quote ,P1)) (y-flip (y-P (quote ,P1)))
			    )
		options)))


;;;;;;;
;; neg-P
;; Input: P [P] "point"
;; Returns: Point which is the negation of P, i.e., -P
;;;;;;;

(define (neg-P P)
  (make-P (- (x-P P))
	  (- (y-P P))))

;;;;;;;
;; add-P

;; Input: P0 [P] "first point"; P1 [P] "second point"
;; Returns: Point sum of P0 and P1, i.e., P0+P1
;;;;;;;

(define (add-P P0 P1)
  (make-P (+ (x-P P0) (x-P P1))
	  (+ (y-P P0) (y-P P1))))


;;;;;;;
;; sub-P
;; Input: P0 [P] "first point"; P1 [P] "second point"
;; Returns: Point difference of P0 and P1, i.e., P0-P1
;;;;;;;

(define (sub-P P0 P1)
  (add-P P0 (neg-P P1)))


;;;;;;;;;
;; scale-P
;; Input: scale[int] "scale factor"; P [P] "point"
;; Returns: Point which is scale of P by scale
;;;;;;;;;

(define (scale-P scale P)
  (make-P (* scale (x-P P))

	  (* scale (y-P P))))

;;;;;;;
;; avg-P
;; Input: P0 [P] "first point"; P1 [P] "second point"
;; Returns: Point which is average of P0 and P1
;;;;;;;

(define (avg-P P0 P1)

  (scale-P 1/2 (add-P P0 P1)))

;;;;;;;;;;
;; rot-90-P
;; Input: P [P] "point"
;; Returns: Point which is P rotated about origin by -90 degrees
;;;;;;;;;;

(define (rot-90-P P)
  (make-P (y-P P)

	  (- (x-P P))))

;;;;;;;;;;
;; coord-P
;;
;; Input: P0 [P] "point simulating origin"
;;        P1 [P] "point simulating the point [0,1]"
;;         v [V] "Vector input"

;; Returns: Point which is the result of thinking of v
;;          in the new coordinate system defined by P0
;;          as the origin and P1 as 1 unit up the y axis
;;
;; This is a VERY powerful function, allowing you to 

;; define new points with almost no need for any 
;; standard graphics geometry (dot product, etc)
;;
;; Example: (avg-P P0 P1) == (coord-P P0 P1 (make-V 0 .5))
;;;;;;;;

(define (coord-P P0 P1 V)
  (let* ((yV (sub-P P1 P0))   ;; the new y axis

         (xV (rot-90-P yV)))  ;; the new x axis
    (add-P P0
	   (add-P (scale-P (x-V V) xV)
		  (scale-P (y-V V) yV) ))))

;;;;;;;;;;
;; coord-Pp
;;
;; Input: P0 [P] "point simulating origin"

;;        P1 [P] "point simulating the point [0,1]"
;;        Vp [Vp] "Polar Vector input, theta in degrees"
;; Returns: Point which is the result of thinking of the polar vector Vp
;;          in the new coordinate system defined by P0

;;          as the origin and P1 as 1 unit up the y axis, 0 degrees
;;          being the point (0,1) (straight up), and degrees counting 
;;          counter-clockwise (that part as in normal polar coordinates).

;;
;; This is a VERY powerful function, allowing you to 
;; define new points with almost no need for any 
;; standard graphics geometry (dot product, etc)
;;
;; Example: (avg-P P0 P1) == (coord-P P0 P1 (make-Vp 0.5 0))

;;;;;;;;

(define (coord-Pp P0 P1 Vp)
  (let* ((yV (sub-P P1 P0))    ;; the new y axis
         (xV (rot-90-P yV)))   ;; the new x axis
    (add-P P0
	   (add-P (scale-P (* -1 (r-Vp Vp) (sindeg (theta-Vp Vp))) xV)

		  (scale-P (*  1 (r-Vp Vp) (cosdeg (theta-Vp Vp))) yV) ))))

;;;;;;;;;;
;; for-each-2
;;
;; Input: f [binary function] 
;;        L [list]
;; Requires: L have at least 2 elements
;; Side-effects: Calls f on first two elts of L, then 

;;               elements 2 and 3, etc.
;; Returns: done
;; Example: (for-each-2 (lambda (a b) (display (word "[" a "," b "]"))(newline)) 
;;                      '(a b c d))

;; ==P [a,b]
;; ==P [b,c]
;; ==P [c,d]
;; ==> done
;;;;;;;;

(define (for-each-2 f L)
  (if (null? (cdr L)) ;; only 1 left
      'done
      (begin (f (car L) (cadr L))
	     (for-each-2 f (cdr L)))))
