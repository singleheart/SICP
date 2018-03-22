(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (e) (cons (car s) e)) rest)))))

(subsets (list 1 2 3))