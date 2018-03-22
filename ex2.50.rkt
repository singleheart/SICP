(define (flip-vert painter)
  ((transform-painter (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))
  painter)) ; new end of edge2

(define (flip-horiz painter)
  ((transform-painter 
    (make-vect 1.0 0.0)   ; new origin
    (make-vect 0.0 0.0)   ; new end of edge1
    (make-vect 1.0 1.0))
   painter)) ; new end of edge2

(define (rotate90 painter)  
  ((transform-painter 
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0))
   painter))

(define (rotate180 painter)
  ((transform-painter                     
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0))
   painter))

(define (rotate270 painter)
  ((transform-painter 
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0))
   painter))