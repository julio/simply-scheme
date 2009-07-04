(define (leapyears-to-year year)
  (+
    (truncate (/ year 4)) 
    (- (truncate (/ year 400)) 
       (truncate (/ year 100)))))

(define (leap-years-between year1 year2)
  (let 
    ((inc (if (leap-year? year1) 1 0)))
    (+
      (- (leapyears-to-year year2) (leapyears-to-year year1))
      inc)))
