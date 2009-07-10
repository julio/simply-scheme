;;;;------------
(define (compressed sent)
  (cond
    ((empty? sent) '())                                ;; empty, nothing to compress
    ((empty? (bf sent)) sent)                          ;; only one digit, don't compress, return that
    ;; HACK to remove first and last, which make their way to the results. good enough for now.
    (else (bf (bl (compress-data sent (se (first sent) '1))))))) ;; start with a count of 1 for the first digit

;; sent contains the string to compress
;; compressed-sent contains the new compressed string
(define (compress-data sent compressed-sent)
  (cond 
    ((empty? sent) compressed-sent) ;; if we're done, compressed-sent contains the output, return that
    ;; last digit?
    ((empty? (bf sent)) (se compressed-sent (first sent) '1))
    ;; same group still ? then call compress-data again, while updating the compress-sent result
    ((eq? (first sent) (first (bf sent))) (compress-data (bf sent) (se (bl compressed-sent) (+ 1 (last compressed-sent)))))
    ;; current letter not the same as the next one, 
    (else (compress-data (bf sent) (se compressed-sent (se (first sent) '1))))))

;;; TODO
;;; fix bf bl hack
;;; do not count the single digits
