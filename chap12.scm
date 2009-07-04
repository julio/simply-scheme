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

;;; exaggerate statements

(define (exaggerate-item wd)
  (cond ((number? wd) (* wd 2))
        ((eq? 'good wd) 'great)
        ((eq? 'bad wd) 'terrible)
        (else wd)))

(define (exaggerate sent)
  (if (eq? sent '())
    sent
    (se (exaggerate-item (first sent)) (exaggerate (bf sent)))))