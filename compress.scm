(define (compressed sent)
  (cond ((empty? sent) '())
        ((empty? (bf sent)) sent)
        (else (compress-data (bf sent) (se '1 (first sent))))))

(define (compress-data sent compressed-sent)
  (cond ((empty? sent) compressed-sent)
        ((eq? (first sent) (last compressed-sent))
          (compress-data 
            (bf sent) 
            (se (bl (bl compressed-sent)) (+ 1 (last (bl compressed-sent))) (last compressed-sent)) ))
        (else 
          (compress-data
            (bf sent) 
            (se compressed-sent (se '1 (first sent)))))))

;;; TODO
;;; do not count the single digits
