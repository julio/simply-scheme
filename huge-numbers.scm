;;; converts a number of plain english. goes all the way to decillion
(define (huge-number n)
  (let ((numbers (number-to-groups-of-three n)))
    (huge-number-helper numbers (count numbers))))

;;; traverses a list of 3 digit groups, converting each group to 
;;; words, based on each group's position
(define (huge-number-helper groups-of-numbers number-of-groups)
  (if (<= (count groups-of-numbers) 1)
    (say-three-digit-number (first groups-of-numbers)) ;; "first" to get the actual word (number), not the sentence
    (se
      (say-three-digit-number (first groups-of-numbers))
      (big-names (first groups-of-numbers) number-of-groups)
      (huge-number-helper 
        (bf groups-of-numbers) 
        (count (bf groups-of-numbers))))))

;;; order of magnitude based on position
(define (big-names n position)
  (if (= 0 (to-number n))
    '()
    (item position '(nothing thousand million billion trillion quadrillion sextillion octillion nonillion decillion))))

;;; convert a number to a sentence of groups of 3 digits
(define (number-to-groups-of-three n)
  (if (<= (count n) 3)
    (se n)
    (se (number-to-groups-of-three (bl (bl (bl n)))) (word (last (bl (bl n))) (last (bl n)) (last n)))))

;;; Say a 3 digit number
(define (say-three-digit-number sent)
  (cond ((< sent 1)    '())
        ((< sent 10)   (se (say-singles sent)))
        ((< sent 100)  (se (say-tens (to-number sent))))
        ((< sent 1000) (se (say-singles (first sent)) 'hundred (say-three-digit-number (bf sent))))
        (else (display-error 1 sent))))

;;; Say 1, 2, ... 9
(define (say-singles n)
  (cond ((eq? '0 (to-number n)) '())
        ((eq? '1 (to-number n)) 'one)
        ((eq? '2 (to-number n)) 'two)
        ((eq? '3 (to-number n)) 'three)
        ((eq? '4 (to-number n)) 'four)
        ((eq? '5 (to-number n)) 'five)
        ((eq? '6 (to-number n)) 'six)
        ((eq? '7 (to-number n)) 'seven)
        ((eq? '8 (to-number n)) 'eight)
        ((eq? '9 (to-number n)) 'nine)
        (else (display-error 2 n))))

;;; Say 10, 11, ..., 20, 21, ... 99
(define (say-tens nn)
  (cond ((eq? '0 (first nn)) '())
        ((eq? '1 (first nn)) (say-teens nn))
        ((eq? '2 (first nn)) (se 'twenty  (say-singles (bf nn))))
        ((eq? '3 (first nn)) (se 'thirty  (say-singles (bf nn))))
        ((eq? '4 (first nn)) (se 'forty   (say-singles (bf nn))))
        ((eq? '5 (first nn)) (se 'fifty   (say-singles (bf nn))))
        ((eq? '6 (first nn)) (se 'sixty   (say-singles (bf nn))))
        ((eq? '7 (first nn)) (se 'seventy (say-singles (bf nn))))
        ((eq? '8 (first nn)) (se 'eighty  (say-singles (bf nn))))
        ((eq? '9 (first nn)) (se 'ninety  (say-singles (bf nn))))
        (else (display-error 3 nn))))

;;; Say 10, 11, 12, ... 19
(define (say-teens nn)
  (cond ((eq? '10 nn) 'ten)
        ((eq? '11 nn) 'eleven)
        ((eq? '12 nn) 'twelve)
        ((eq? '13 nn) 'thirteen)
        ((eq? '14 nn) 'forteen)
        ((eq? '15 nn) 'fifteen)
        ((eq? '16 nn) 'sixteen)
        ((eq? '17 nn) 'seventeen)
        ((eq? '18 nn) 'eighteen)
        ((eq? '19 nn) 'nineteen)
        (else (display-error 4 nn))))


;;; TODO
;;;  numbers are sometimes "000" instead of '000 or (000) and need to add 0 to fix
;;;  big names need to offset first position. using "nothing" for now

;;; some of the groups go from 'nnn to "nnn" which screws up comparisons.
;;; adding 0 to "nnn" returns 'nnn, which helps. Ideally we would not need
;;; to do this.
(define (to-number n)
  (+ 0 n))

(define (display-error number msg)
  (se '/error number msg '/))
