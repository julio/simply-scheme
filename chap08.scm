;;; define count using high order functions

(define (one-item item) 1)

(define (count-items sent)
  (accumulate + (every one-item sent)))
  
;;; define acronym using high order functions

(define (real-word? wd)
  (not (member? wd '(the and or a an))))
  
(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))
  
;;; pig latin to entire sentence

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (pigl wd)
  (if (vowel? (first wd))
    (word wd 'ay)
  (pigl (word (bf wd) (first wd)))))
  
(define (piglatin-all sent)
  (every pigl sent))

;;; 8.4 choose beatles

(define (choose-beatles condition)
  (keep condition '(john paul ringo george)))

(define (ends-vowel? beatle)
  (vowel? (last beatle)))
  
;;; 8.5 transform beatles

(define (transform-beatles transformation)
(every transformation '(john paul ringo george)))

(define (title beatle)
(word 'Sir- beatle))

;;; 8.6 phonetic

(define (words wd)
  (every phonetic wd))

(define (phonetic letter)
  (cond 
    ((eq? letter 'A) 'Alpha)
    ((eq? letter 'B) 'Bravo)
    ((eq? letter 'C) 'Charlie)
    ((eq? letter 'D) 'Delta)
    ((eq? letter 'E) 'Echo)
    ((eq? letter 'F) 'Foxtrot)
    ((eq? letter 'G) 'Golf)
    ((eq? letter 'H) 'Hotel)
    ((eq? letter 'I) 'India)
    ((eq? letter 'J) 'Juliet)
    ((eq? letter 'K) 'Kilo)
    ((eq? letter 'L) 'Lima)
    ((eq? letter 'M) 'Mike)
    ((eq? letter 'N) 'November)
    ((eq? letter 'O) 'Oscar)
    ((eq? letter 'P) 'Papa)
    ((eq? letter 'Q) 'Quebec)
    ((eq? letter 'R) 'Romeo)
    ((eq? letter 'S) 'Sierra)
    ((eq? letter 'T) 'Tango)
    ((eq? letter 'U) 'Uniform)
    ((eq? letter 'V) 'Victor)
    ((eq? letter 'W) 'Whiskey)
    ((eq? letter 'X) 'X-ray)
    ((eq? letter 'Y) 'Yankee)
    ((eq? letter 'Z) 'Zulu)))
    