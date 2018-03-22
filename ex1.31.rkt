(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
  (if (= n 0)
      0
      (product identity 1 inc n)))
(define (even? x)
  (= (remainder x 2) 0))
(define (approx-pi n)
  (define (numerator n)
    (define (term x)
      (if (even? x)
          (+ x 2)
          (+ x 3)))
    (product term 0 inc n))
  (define (denominator n)    
    (define (term x)
      (if (even? x)
          (+ x 3)
          (+ x 2)))
    (product term 0 inc n))
  (* 4.0 (/ (numerator n) (denominator n))))