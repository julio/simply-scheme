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
  (cond ((eq? 'M roman-numeral) 1000)
        ((eq? 'D roman-numeral) 500)
        ((eq? 'C roman-numeral) 100)
        ((eq? 'L roman-numeral) 50)
        ((eq? 'X roman-numeral) 10)
        ((eq? 'V roman-numeral) 5)
        ((eq? 'I roman-numeral) 1)
        (else 0)))

;;; describe-time
;;; (describe-time 22222) => (6 hours 10 minutes 22 seconds)
;;; (describe-time 4967189641) => (1 centuries 57 years 20 weeks 6 days 8 hours 54 minutes 1 seconds)

(define (describe-time seconds)
 ;;;
)