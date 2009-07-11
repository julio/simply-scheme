(define (compressed sent)
  (if (<= (count sent) '1)
    sent
    (compress-data (bf sent) (se '1 (first sent)))))

(define (compress-data sent compressed-sent)
  (cond ((empty? sent) compressed-sent)
        ((eq? (first sent) (last compressed-sent))
          (compress-data
            (bf sent) (se (bl (bl compressed-sent)) (+ 1 (last (bl compressed-sent))) (last compressed-sent)) ))
        (else 
          (compress-data
            (bf sent) (se compressed-sent (se '1 (first sent)))))))

(define (remove-single-counts sent)
  (cond ((eq? '1 (count sent)) sent)
        ((eq? (first sent) '1) (se (first (bf sent))(remove-single-counts (bf sent))))
        (else (se (remove-single-counts (bf (bf sent)))))))
        
;;; TODO
;;; do not count the single digits
