(let ((id (lambda (x) x)))

 (letrec ((f (lambda (n)
               (if (<= n 0)
                   1
                   (* n (f (- n 1))))))

          (g (lambda (n)
               (if (<= n 1)
                   1
                   (* n (g (- n 1)))))))

   (+ ((id f) 3) ((id g) 4))))