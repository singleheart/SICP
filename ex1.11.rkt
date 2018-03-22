(define (f n) 
  (define (f-recur n)
    (if (< n 3)
        n
        (+ (f-recur (- n 1))         
           (* 2 (f-recur (- n 2)))
           (* 3 (f-recur (- n 3))))))
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter b
                c
                (+ (* 3 a) (* 2 b) c)
                (- count 1))))
  (f-iter (/ 1 9) (/ 1 3) 0 n)
  )