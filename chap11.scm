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
