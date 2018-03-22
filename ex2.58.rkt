(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))
(define (expt b n)
  (define (iter-expt prod base n)
    (cond ((<= n 0) prod)
          ((even? n) (iter-expt prod (square base) (/ n 2)))
          (else (iter-expt (* prod base) base (- n 1)))))
  (iter-expt 1 b n))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (triple? x)
  (and (pair? x) (pair? (cdr x)) (pair? (cddr x))))

(define (sum? x)
  (and (list? x) (memq '+ x)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (memq-r item x)
  (cond ((null? x) nil)
        ((eq? item (car x)) nil)
        (else (cons (car x) (memq-r item (cdr x))))))

(define (single-list? x)
  (and (list? x) (= (length x) 1)))

(define (contract-single-list x)
  (if (single-list? x)
      (car x)
      x))

(define (addend s)
  (contract-single-list (memq-r '+ s)))
  
(define (augend s) 
  (contract-single-list (cdr (memq '+ s))))

(define (product? x)
  (and (triple? x) (eq? (cadr x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (contract-single-list (cddr p)))


(define (exponentiation? x)
  (and (triple? x) (eq? (cadr x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list base '** exponent))))

(define (base e) (car e))

(define (exponent e) (caddr e))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product 
           (make-exponentiation (base exp)
                                (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))