(define (reverse list)
  (if (null? list)
      list
      (append (reverse (cdr list)) 
              (cons (car list) nil))))
(define (deep-reverse list)
  (cond ((null? list) list)
        ((not (pair? list)) list)
        (else (append (deep-reverse (cdr list)) 
              (cons (deep-reverse (car list)) nil)))))

(define x (list (list 1 2) (list 3 4)))

x
(reverse x)
(deep-reverse x)