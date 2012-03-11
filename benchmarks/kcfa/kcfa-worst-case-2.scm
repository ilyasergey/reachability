; n = 2
; # terms = 69
((lambda (f1) (f1 #t) (f1 #f)) (lambda (x1) ((lambda (f2) (f2 #t) (f2 #f)) (lambda (x2) ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))
