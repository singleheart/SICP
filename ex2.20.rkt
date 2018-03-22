(define (same-parity . l)
  (define pivot (car l))
  (define (iter result rest)
    (if (null? rest)
        result
        (if (even? (+ pivot (car rest)))
            (iter (cons result (car rest)) (cdr rest))
            (iter result (cdr rest)))))
  (iter (list (car l)) (cdr l)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)