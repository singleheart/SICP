(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (average a b)
  (/ (+ a b) 2.0))
(define (midpoint-segment segment)
  (define (average-point point-selector segment)
    (average (point-selector (start-segment segment)) (point-selector (end-segment segment))))
  (define (average-x segment)    
    (average-point x-point segment))
  (define (average-y segment)
    (average-point y-point segment))
  
  (make-point (average-x segment) (average-y segment)))
(define origin (make-point 0 0))
(define pointA (make-point 2 4))
(define (less x y)
  (if (< x y)
     x
     y))
(define (greater x y)
  (if (< x y)
     y
     x))
(define (make-rectangle first-point second-point)
  (let ((first-x (x-point first-point))
        (first-y (y-point first-point))
        (second-x (x-point second-point))
        (second-y (y-point second-point)))
    (cons (make-point (less first-x second-x) (less first-y second-y))
         (make-point (greater first-x second-x) (greater first-y second-y)))))
(define (start-x rectangle)
  (caar rectangle))
(define (start-y rectangle)
  (cdar rectangle))
(define (end-x rectangle)
  (cadr rectangle))
(define (end-y rectangle)
  (cddr rectangle))
(define (height rectangle)
  (abs (- (end-y rectangle) (start-y rectangle))))
(define (width rectangle)
  (abs (- (end-x rectangle) (start-x rectangle))))
(define (perimeter rectangle)
  (* 2 (+ (height rectangle) (width rectangle))))
(define (area rectangle)
  (* (height rectangle) (width rectangle)))