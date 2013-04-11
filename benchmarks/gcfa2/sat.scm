(define (phi x1 x2)
  (and (or x1 (not x2))
       (or (not x2))
       (or x1 x2)))

(define (try f)
  (or (f #t) (f #f)))

(define (sat-solve-4 p)
  (try (lambda (n1)
         (try (lambda (n2)
                (p n1 n2))))))

                        
(sat-solve-4 phi)