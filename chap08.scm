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