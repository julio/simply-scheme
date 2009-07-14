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

(define (n? wd)
  (cond ((= 0 (count wd)) #t) ;; not good actually, as (n? '()) returns true
        ((member? (first wd) '(0 1 2 3 4 5 6 7 8 9)) (and #t (n? (bf wd))))
        (else #f)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((eq? wd (first sent)) (bf sent))
        (else (se (first sent) (remove-once wd (bf sent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-sorted-lists list1 list2)
  (cond ((empty? list1) list2)
        ((empty? list2) list1)
        ((<= (first list1) (first list2)) (se (first list1) (merge-sorted-lists (bf list1) list2)))
        (else (se (first list2) (merge-sorted-lists list1 (bf list2))))))
