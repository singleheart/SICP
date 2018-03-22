(define (outline->painter)
  (define ll (make-vect 0 0))
  (define lr (make-vect 0 1))
  (define ul (make-vect 1 0))
  (define ur (make-vect 1 1))
  
  (define seg1 (make-segment ll ul))
  (define seg2 (make-segment ul ur))
  (define seg3 (make-segment ur lr))
  (define seg4 (make-segment lr ll))
  
  (segments->painter (list seq1 seq2 seq3 seq4)))

(define (x->painter)
  (define ll (make-vect 0 0))
  (define lr (make-vect 0 1))
  (define ul (make-vect 1 0))
  (define ur (make-vect 1 1))
  
  (define seg1 (make-segment ll ur))
  (define seg2 (make-segment ul lr))
  
  (segments->painter (list seq1 seq2)))

(define (diamond->painter)
  (define l (make-vect 0 0.5))
  (define r (make-vect 1 0.5))
  (define u (make-vect 0.5 1))
  (define d (make-vect 0.5 0))
  
  (define seg1 (make-segment l u))
  (define seg2 (make-segment u r))
  (define seg3 (make-segment r d))
  (define seg4 (make-segment d l))
  
  (segments->painter (list seq1 seq2 seq3 seq4)))

(define (wave->painter)
  (segments->painter (list
                      ; head
                      ; left head
                      (make-segment
                       (make-vect 0.4 1.0)
                       (make-vect 0.35 0.8))
                      (make-segment                       
                       (make-vect 0.35 0.8)
                       (make-vect 0.4 0.6))
                      ; right head
                      (make-segment
                       (make-vect 0.6 1.0)
                       (make-vect 0.65 0.8))
                      (make-segment                       
                       (make-vect 0.65 0.8)
                       (make-vect 0.6 0.6))
                      ; left shoulder
                      (make-segment                                              
                       (make-vect 0.4 0.6)
                       (make-vect 0.3 0.6))
                      ; left arm
                      (make-segment
                       (make-vect 0.3 0.6)
                       (make-vect 0.15 0.55))
                      (make-segment
                       (make-vect 0.15 0.55)
                       (make-vect 0.0 0.8))
                      (make-segment                       
                       (make-vect 0.0 0.6)
                       (make-vect 0.15 0.35))
                      (make-segment                                              
                       (make-vect 0.15 0.35)
                       (make-vect 0.3 0.55))
                      ;left armpit
                      (make-segment                                                                     
                       (make-vect 0.3 0.55)
                       (make-vect 0.35 0.45))
                      ; left leg
                      (make-segment
                       (make-vect 0.35 0.45)
                       (make-vect 0.2 0.0))
                      (make-segment
                       (make-vect 0.4 0.0)
                       (make-vect 0.5 0.2))
                      ; right leg
                      (make-segment
                       (make-vect 0.5 0.2)
                       (make-vect 0.6 0.0))
                      (make-segment
                       (make-vect 0.8 0.0)
                       (make-vect 0.65 0.4))
                      ; right arm
                      (make-segment                       
                       (make-vect 0.65 0.4)
                       (make-vect 1.0 0.1))
                      (make-segment                       
                       (make-vect 1.0 0.3)
                       (make-vect 0.7 0.6))
                      ; right shoulder
                      (make-segment                                              
                       (make-vect 0.6 0.6)
                       (make-vect 0.7 0.6))                      
                      )))
  