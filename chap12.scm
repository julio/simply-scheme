;;; reverse

(define (reverse wd)
  (if (= 1 (count wd))
    wd
    (word (last wd) (reverse (bl wd)))))

;;; factorial

(define (factorial n)
  (if (= 1 n)
    1
    (* n (factorial (- n 1)))))

;;; evens

(define (evens sent)
  (if (< (count sent) 2)
    '()
    (se (first (bf sent)) (evens (bf (bf sent))))))
