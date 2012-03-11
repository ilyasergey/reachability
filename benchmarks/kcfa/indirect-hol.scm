(define (do-something) 
  10)

(define (id y) 
  (do-something)
  y)

(id #t)
(id #f)