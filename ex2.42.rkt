(define (filter predicate sequence)
  (cond ((null? sequence) nil)
       ((predicate (car sequence))        
        (cons (car sequence)
             (filter predicate (cdr sequence))))
       (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
     initial
     (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
     nil
     (cons low (enumerate-interval (+ low 1) high))))

(define empty-board nil)

(define (adjoin-position new-row new-column rest-of-queens)
  (cons (cons new-row new-column) rest-of-queens))

(define (safe? k positions)  
  (define (ok? position)
    (and
     (not (= (caar positions) (car position)))
     (not (= 
           (abs (- (caar positions)(car position))) 
           (abs (- (cdar positions)(cdr position)))))))     
  (define (and-function x y) (and x y))
  (accumulate and-function #t (map ok? (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
       (list empty-board)
       (filter
        (lambda (positions) (safe? k positions))
        (flatmap
         (lambda (rest-of-queens)
           (map (lambda (new-row)
                  (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))         
         (queen-cols (- k 1))))))
  (queen-cols board-size))