(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode-symbol symbol tree)
  (define (match-symbol? symbol tree)
    (element-of-set? symbol (symbols tree)))  
  (cond 
    ((leaf? tree) '())
    ((match-symbol? symbol (left-branch tree)) 
     (cons '0 (encode-symbol symbol (left-branch tree))))
    ((match-symbol? symbol (right-branch tree)) 
     (cons '1 (encode-symbol symbol (right-branch tree))))
    (else (error "symbol not in the tree" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (define (successive-merge set)
    (cond ((< (length set) 1) '())
          ((= (length set) 1) (car set))
          (else
           (successive-merge (adjoin-set 
                              (make-code-tree (car set) (cadr set))  
                              (cddr set))))))
  (successive-merge (make-leaf-set pairs)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define symbol-frequency-table
  '((a 2) (na 16) (boom 1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1)))

(define huffman-tree
  (generate-huffman-tree symbol-frequency-table))

(define original-message '(
                           Get a job
                               
                               Sha na na na na na na na na
                               
                               Get a job
                               
                               Sha na na na na na na na na
                               
                               Wah yip yip yip yip yip yip yip yip yip
                               
                               Sha boom))

(length (encode original-message huffman-tree))