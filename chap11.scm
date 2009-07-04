;;; (downup 'foo) => (foo fo f fo foo)

(define (downup wd)
  (if (= (count wd) 1)
    (se wd)
    (se wd (downup (bl wd)) wd)))

;;; is the letter a vowel?

(define (vowel? ltr)
  (member? ltr '(a e i o u)))
  
;;; (pigl 'word) => ordway

(define (pigl wd)
  (if (vowel? (first wd))
    (word wd 'ay)
    (pigl (word (bf wd) (first wd)))))
    
;;; (explode 'foo) => f o o

(define (explode wd)
  (if (= 0 (count wd))
    '()
    (se (first wd) (explode (bf wd)))))

;;; (letter-pairs wd) => (letter-pairs 'foo) => fo o

(define (letter-pairs wd)
  (if (< (count wd) 3)
    wd
    (se (word (first wd) (first (bf wd))) (letter-pairs (bf (bf wd))))))
    
;;; (count-ums sent) => counts the number of times some says "um"

(define (count-ums sent)
  (if (= 0 (count sent))
    0
    (let ((inc (if (eq? 'um (first sent)) 1 0)))
      (+ inc (count-ums (bf sent))))))
    
;;; phone-unspell
;;;
;;; (phone-unspell 'popcorn) => 7672676

(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
        ((member? letter 'def) 3)
        ((member? letter 'ghi) 4)
        ((member? letter 'jkl) 5)
        ((member? letter 'mno) 6)
        ((member? letter 'prs) 7)
        ((member? letter 'tuv) 8)
        ((member? letter 'wxyz) 9)
        (else 0)))
        
(define (phone-unspell wd)
  (if (= 0 (count wd))
    wd
    (word (unspell-letter (first wd)) (phone-unspell (bf wd)))))

;;; initials
;;;
;;; (initials '(the beatles)) => (T B)

(define (initials sent)
  (if (eq? '() sent)
    sent
    (se (first (first sent)) (initials (bf sent)))))
    
;;; copies
;;;
;;; (copies 3 'spam) => (spam spam spam)

(define (copies n wd)
  (if (= 0 n)
    '()
    (se wd (copies (- n 1) wd))))
