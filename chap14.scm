;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sent-max sent)
  (if (= 1 (count sent))
    (first sent)
    (max (first sent) (sent-max (bf sent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pairs wd)
  (if (<= (count wd) 1)
    wd
    (pairs-helper wd wd)))

(define (pairs-helper wd all-letters)
  (if (empty? wd)
    '()
    (se (each-letter (first wd) all-letters) (pairs-helper (bf wd) all-letters) )))
  
(define (each-letter letter all-letters)
  (if (= 0 (count all-letters))
    '()
    (se (word letter (first all-letters)) (each-letter letter (bf all-letters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((member? ))
    (remove-once-helper wd sent)))
    
(define (remove-once-helper wd sent)
  (if (empty? sent)
    '()
    (se )