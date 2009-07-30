(define (devilish x y)
  (let ((z (x y)))
    (lambda (w) (keep (lambda (a) (equal? z a)) w))))