;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NAME:        fractals.scm
;;
;; DESCRIPTION: Six fractals using gdraw graphics
;; 
;; AUTHOR:      Dan Garcia  -  University of California, Berkeley

;;              Copyright (C) 2008. All rights reserved.
;;
;; DATE:        2008-11-15
;;
;; UPDATE HIST:  
;;
;; 2008-11-15 v1.0  : Release 1.0
;; 2009-04-15 v2.0  : Added demo-all-fractals; changed tree to twig

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the helpers

(load "~/Desktop/fractal/gdraw")
(load "~/Desktop/fractal/gdraw-helpers")

;; Make a new window, set its size

(init-graphics)

(set-canvas-size *R* *U*)

;;;;;;;;;;;;;;;;;;;;
;; SIERPINSKI-SQUARE
;;;;;;;;;;;;;;;;;;;;

(define (draw-square L D R U)
  (draw-rectangle L (y-flip D)
		  R (y-flip U) 
		  'outline 'blue 'fill 'red))


(define (draw-sierpinski-square L D R U n)
  (if (= n 0) 
      (draw-square L D R U)
      (let ((Lish (lerp L R 1/3))
            (Rish (lerp L R 2/3))
            (Dish (lerp D U 1/3))
            (Uish (lerp D U 2/3)))

        (draw-sierpinski-square L    D    Lish Dish (- n 1))
        (draw-sierpinski-square Rish D    R    Dish (- n 1))
        (draw-sierpinski-square L    Uish Lish U    (- n 1))
        (draw-sierpinski-square Rish Uish R    U    (- n 1))

        (draw-sierpinski-square Lish Dish Rish Uish (- n 1)) )))

;; DEMO-SIERPINSKI-SQUARE

(define (demo-sierpinski-square)
  (read-char)
  (demo-sierpinski-square-helper 0 5))
(define (demo-sierpinski-square-helper n n-finish)

  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))
       (read-char)
       (clear-graphics!)
       (draw-sierpinski-square *L* (+ *D* 1) (- *R* 1) *U* n)

       (demo-sierpinski-square-helper (+ 1 n) n-finish))))

;; (clear-graphics!)
;; (draw-sierpinski-square *L* *D* *R* *U* 4)
;; (demo-sierpinski-square)

;;;;;;;;;;;;;;;;;;;;;;
;; SIERPINSKI TRIANGLE

;;;;;;;;;;;;;;;;;;;;;;

(define (draw-triangle L D R U)
  (draw-polygon L              (y-flip D) 
                R              (y-flip D) 
                (lerp L R 1/2) (y-flip U)
                L              (y-flip D)  

		'outline 'blue 'fill 'red))

(define (draw-sierpinski-triangle L D R U n)
  (if (= n 0)
      (draw-triangle L D R U)
      (let ((LR   (lerp L R 1/2))
            (Lish (lerp L R 1/4))

            (Rish (lerp L R 3/4))
            (DU   (lerp D U 1/2)))
        (draw-sierpinski-triangle L    D  LR   DU (- n 1))
        (draw-sierpinski-triangle LR   D  R    DU (- n 1))
        (draw-sierpinski-triangle Lish DU Rish U  (- n 1)) )))



;; DEMO-SIERPINSKI-TRIANGLE

(define (demo-sierpinski-triangle)
  (display "Sierpinski Triangle Demo")(newline)
  (read-char)
  (demo-sierpinski-triangle-helper 0 7))
(define (demo-sierpinski-triangle-helper n n-finish)

  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))
       (read-char)
       (clear-graphics!)
       (draw-sierpinski-triangle *L* (lerp *U* *D* *sqrt3/2*) *R* *U* n)

       (demo-sierpinski-triangle-helper (+ 1 n) n-finish))))

;; (clear-graphics!)
;; (draw-sierpinski-triangle *L* (lerp *U* *D* *sqrt3/2*) *R* *U* 1)
;; (demo-sierpinski-triangle)

;;;;;;;
;; TREE

;;;;;;;

(define (draw-tree L D R U)
  (let ((Lish (lerp L R 1/4))
        (LR   (lerp L R 1/2))
        (Rish (lerp L R 3/4)))
    (draw-line Lish (y-flip D) 
               LR   (y-flip U) 
               Rish (y-flip D) 

	       'fill 'blue 'width 2)))

(define (draw-fractal-tree L D R U n)
  (if (= n 1)
      (draw-tree L D R U)
      (let ((DUn  (lerp U D (/ n)))
            (LR   (lerp L R 1/2)))
        (draw-tree L DUn R U)

        (draw-fractal-tree L  D LR DUn (- n 1))
        (draw-fractal-tree LR D R  DUn (- n 1)))))

;; DEMO-TREE

(define (demo-tree)
  (display "Tree Demo")(newline)
  (read-char)
  (demo-tree-helper 1 7))

(define (demo-tree-helper n n-finish)
  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))
       (read-char)
       (clear-graphics!)
       (draw-fractal-tree *L* *D* *R* *U* n)

       (demo-tree-helper (+ 1 n) n-finish))))

;; (clear-graphics!)
;; (draw-fractal-tree *L* *D* *R* *U* 7)
;; (demo-tree)

;;;;;;;
;; TWIG
;;;;;;;

(define (draw-twig P0 P1 n)
  (if (= n 0)

      (draw-line-P P0 P1 "fill" "blue" "width" 1 "capstyle" "round")
      (let* ((Pm (coord-Pp P0 P1 (make-Vp 1/2  0)))
	     (Pl (coord-Pp Pm P1 (make-Vp  1  20)))

	     (Pr (coord-Pp Pm P1 (make-Vp  1 -20))))
	(draw-line-P P0 Pm "fill" "blue" "width" (+ (* 2 n) 1) "capstyle" "round")
	(draw-twig Pm Pl (- n 1))
	(draw-twig Pm Pr (- n 1)))))


;; DEMO TWIG

(define (demo-twig)
  (display "Twig Demo")(newline)
  (read-char)
  (demo-twig-helper 0 7))
(define (demo-twig-helper n n-finish)
  (if (> n n-finish)
      'done

      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))
       (read-char)
       (clear-graphics!)
       (draw-twig (make-P (lerp *L* *R* 1/2) *D*) 
		  (make-P (lerp *L* *R* 1/2) *U*) n)

       (demo-twig-helper (+ 1 n) n-finish))))

;; (clear-graphics!)
;; (draw-twig (make-P (lerp *L* *R* 1/2) *D*) (make-P (lerp *L* *R* 1/2) *U*) 7)
;; (demo-twig)

;;;;;;;;
;; PEANO
;;;;;;;;


(define (draw-peano P0 P1 n)
  (if (= n 0)
      (draw-line-P P0 P1 "fill" "blue" "width" 8 "capstyle" "round")
      (let ((P0C (coord-P P0 P1 (make-V    0 1/3)))

	    (P0D (coord-P P0 P1 (make-V  1/3 1/3)))
	    (P0U (coord-P P0 P1 (make-V -1/3 1/3)))
	    (P1C (coord-P P0 P1 (make-V    0 2/3)))
	    (P1D (coord-P P0 P1 (make-V  1/3 2/3)))
	    (P1U (coord-P P0 P1 (make-V -1/3 2/3))))

	(for-each-2 (lambda (Pa Pb) (draw-peano Pa Pb (- n 1)))
		    (list P0 P0C P0U P1U P1C P1D P0D P0C P1C P1)))))
        
;; DEMO PEANO

(define (demo-peano)
  (display "Peano Demo")(newline)

  (read-char)
  (demo-peano-helper 0 4))
(define (demo-peano-helper n n-finish)
  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))
       (read-char)

       (clear-graphics!)
       (draw-peano (make-P *L* (lerp *U* *D* 1/2)) 
		   (make-P *R* (lerp *U* *D* 1/2)) n)
       (demo-peano-helper (+ 1 n) n-finish))))

;; (clear-graphics!)
;; (draw-peano (make-P *L* (lerp *U* *D* 1/2)) (make-P *R* (lerp *U* *D* 1/2)) 2)

;; (demo-peano)

;;;;;;;;;;;;;;;;;;;
;; C-CURVE / DRAGON
;;;;;;;;;;;;;;;;;;;

(define (draw-c-curve/dragon-P P0 P1 n dragon?)
  (if (= n 0)
      (draw-line-P P0 P1 "fill" "blue" "width" 3 "capstyle" "round")

      (let ((Pm (coord-P P0 P1 (make-V -1/2 1/2))))
        (draw-c-curve/dragon-P P0 Pm (- n 1) dragon?)
        (if dragon?
            (draw-c-curve/dragon-P P1 Pm (- n 1) dragon?)
            (draw-c-curve/dragon-P Pm P1 (- n 1) dragon?)))))


;; DEMO C-CURVE AND DRAGON

(define (demo-c-curve)
  (display "C-Curve Demo")(newline)
  (read-char)
  (demo-c-curve/dragon-helper 0 13 #f))
(define (demo-dragon)
  (display "Dragon Demo")(newline)

  (read-char)
  (demo-c-curve/dragon-helper 0 13 #t))
(define (demo-c-curve/dragon-helper n n-finish dragon?)
  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))

       (read-char)
       (clear-graphics!)
       (draw-c-curve/dragon-P (make-P (lerp *L* *R* 1/4)
				      (lerp *D* *U* 3/8))
			      (make-P (lerp *L* *R* 3/4)
				      (lerp *D* *U* 3/8))
			      n dragon?)

       (demo-c-curve/dragon-helper (+ 1 n) n-finish dragon?))))

;; (clear-graphics!)
;; (draw-c-curve/dragon-P (make-P (lerp *L* *R* 1/4) (lerp *D* *U* 3/8)) (make-P (lerp *L* *R* 3/4) (lerp *D* *U* 3/8)) 4 #t)

;; (demo-c-curve)
;; (demo-dragon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KOCH (SNOWFLAKE) / ANTI-KOCH (ANTI-SNOWFLAKE) CURVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (draw-koch-edge P0 P1 n)
  (if (= n 0)
      (draw-line-P P0 P1 "fill" "blue" "width" 3 "capstyle" "round")
      (for-each-2 (lambda (Pa Pb) (draw-koch-edge Pa Pb (- n 1)))

		  (list P0
			(coord-P P0 P1 (make-V 0 1/3))                ;; P1/3
			(coord-P P0 P1 (make-V (/ *-sqrt3/2* 3) 1/2)) ;; Pm
			(coord-P P0 P1 (make-V 0 2/3))                ;; P2/3
			P1))))

(define (draw-koch PSW PN PSE n invert?)

  (for-each-2 (lambda (Pa Pb) (draw-koch-edge Pa Pb n))
	      (if invert? 
		  (list PSW PSE PN  PSW)    ;; Go counter-clockwise
		  (list PSW PN  PSE PSW)))) ;; Go clockwise

;; DEMO-KOCH and DEMO-KOCH-INVERTED


(define (demo-koch)
  (display "Koch Curve Demo")(newline)
  (read-char)
  (set-canvas-size *R* (* *U* *sqrt3/2* (/ 4 3)))
  (demo-koch-helper 0 5 #f))
(define (demo-koch-inverted)
  (display "Koch Curve Inverted Demo")(newline)

  (read-char)
  (set-canvas-size *R* *U*)
  (demo-koch-helper 0 5 #t))
(define (demo-koch-helper n n-finish invert?)
  (if (> n n-finish)
      'done
      (begin
       (display `(Hit enter to display frame ,n of ,n-finish))

       (read-char)
       (clear-graphics!)
       (draw-koch (make-P *L* (lerp *U* *D* *sqrt3/2*))
		  (make-P (lerp *L* *R* 1/2) *U*)
		  (make-P *R*  (lerp *U* *D* *sqrt3/2*))
		  n
		  invert?)
       (demo-koch-helper (+ 1 n) n-finish invert?))))


;; (clear-graphics!)
;; (set-canvas-size *R* (* *U* *sqrt3/2* (/ 4 3)))
;; (draw-koch (make-P *L* (lerp *U* *D* *sqrt3/2*)) (make-P (lerp *L* *R* 1/2) *U*) (make-P *R* (lerp *U* *D* *sqrt3/2*)) 3 #f)
;; (set-canvas-size *R* *U*)

;; (draw-koch (make-P *L* (lerp *U* *D* *sqrt3/2*)) (make-P (lerp *L* *R* 1/2) *U*) (make-P *R* (lerp *U* *D* *sqrt3/2*)) 3 #t)
;; 
;; (demo-koch)
;; (demo-koch-inverted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEMO-ALL-FRACTALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (demo-all-fractals)
  (demo-sierpinski-triangle)
  (demo-twig)
  (demo-peano)
  (demo-c-curve)
  (demo-dragon)
  (demo-koch)

  (demo-koch-inverted))
