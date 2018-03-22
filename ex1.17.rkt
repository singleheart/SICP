(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))
(define (* a b)
  (define (fast-mult a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-mult a (halve b))))
          (else (+ a (fast-mult a (- b 1))))))
  (define (mult prod a b)
    (cond ((= b 0) prod)
          ((even? b) (mult prod (double a) (halve b)))
          (else (mult (+ prod a) a (- b 1)))))
  (fast-mult a b))