; n = 2
; # terms = 69
((lambda (f1)
    (let ((a (f1 #t)))
        (f1 #f)))
            (lambda (x1) ((lambda (f2)
                (let ((b (f2 #t)))
                    (let ((c (f2 #f)))
                      (f2 #t))))
                         (lambda (x2) ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))

