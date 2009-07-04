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
    
;;; roman numerals

(define (arabic roman)
  (if (= 0 (count roman))
    0
    (if (and (> (count roman) 1)
             (< (arabic-value (first roman)) (arabic-value (first (bf roman)))))
      (- (arabic (bf roman)) (arabic-value (first roman)))
      (+ (arabic-value (first roman)) (arabic (bf roman))))))

(define (arabic-value roman-numeral)
  (cond ((eq? 'm roman-numeral) 1000)
        ((eq? 'd roman-numeral) 500)
        ((eq? 'c roman-numeral) 100)
        ((eq? 'l roman-numeral) 50)
        ((eq? 'x roman-numeral) 10)
        ((eq? 'v roman-numeral) 5)
        ((eq? 'i roman-numeral) 1)
        (else 0)))
