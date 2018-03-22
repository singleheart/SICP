(define (equal? a b)
  (cond ((null? a) (null? b))
        ((null? b) (null? a)) ; #f
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))