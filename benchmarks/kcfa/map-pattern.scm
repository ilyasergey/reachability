
(define (debug-trace) #f)

(define (id xx)
  (debug-trace)
  xx)


(define (my-map f l)
  (debug-trace)
  (letrec ((lp (lambda (lst)
                 (if (not (pair? lst))
                     '()
                     (cons ((id f) (car lst))
                           (lp (cdr lst)))))))
    (lp l)))


(define ans1 (my-map (id (lambda (a) (+ 1 a))) '(1 2 3)))

(define ans2 (my-map (id (lambda (b) (+ 1 b))) '(7 8 9)))


