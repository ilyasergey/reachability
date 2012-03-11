(define (println s)
  (display s)
  (newline))

(define (phi x1 x2 x3 x4 x5 x6 x7)
  (and (or x1 x2)
       (or x1 (not x2) (not x3))
       (or x3 x4)
       (or (not x4) x1)
       (or (not x2) (not x3))
       (or x4 x2)))

(define (try f)
  (println "trying")
  (or (f #t) (f #f)))

(define (sat-solve-7 p)
  (try (lambda (n1)
         (try (lambda (n2)
                (try (lambda (n3)
                       (try (lambda (n4)
                              (try (lambda (n5)
                                     (try (lambda (n6)
                                            (try (lambda (n7)
                                                   (p n1 n2 n3 n4 n5 n6 n7))))))))))))))))

                        
(display (sat-solve-7 phi))
(newline)

