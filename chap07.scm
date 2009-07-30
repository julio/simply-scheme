;; no lambda

(define (include? a lat)
  (if (empty? lat)
    #f
    (or (eq? (first lat) a)
        (include? a (bf lat)))))

;; ============================================

(define (who sent)
  (every (lambda (person) (se person sent)) '(pete roger john keith)))

;; ============================================

(define (prepend-every wd sent)
  (every (lambda (wd) (word 's wd)) sent))

;; ============================================
