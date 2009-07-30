(define (draw-cesar-curve P0 P1 n)
  (if (= n 0)
      (draw-line-P P0 P1 "fill" "purple" "width" 2 "capstyle" "round")
      (let ((Pm (coord-P P0 P1 (make-V -1/3 1))))
        (and (draw-cesar-curve P1 Pm (- n 1))
         (draw-cesar-curve Pm P0 (- n 1))
         (draw-cesar-curve Pm P1 (- n 1))
         (draw-cesar-curve P0 Pm (- n 1))))))


;; DEMO-FRACTAL

(define (demo-fractal)
  (display "cesar curve")
  (newline)
  (read-char)
  (demo-fractal-helper 0 5))

(define (demo-fractal-helper n n-finish)
  (if (> n n-finish)
      'done
      (begin
    (display `(Hit enter to display frame ,n of ,n-finish))
    (read-char)
    (clear-graphics!)
    (draw-cesar-curve (make-P (lerp *L* *R* 1/4)
                      (lerp *D* *U* 5/8))
                  (make-P (lerp *L* *R* 3/4)
                      (lerp *D* *U* 5/8))
                  n)
    (demo-fractal-helper (+ 1 n) n-finish))))
