((lambda (f) (f 1) (f 2) (f 2) (f 1)) (lambda (x) ((lambda (y) y) 0 )))

;((lambda (f1) (let ((a (f1 #t)))
;        (f1 #f))) (lambda (x1) ((lambda (f2) (let ((a (f2 #t)))
;                                (f2 #f))) (lambda (x2) ((lambda (f3) (let ((a (f3 #t)))
;                                        (f3 #f))) (lambda (x3) ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1))))))))


;;((lambda (f) (let ((a (f 1)))
;;                (let ((a (f 2)))
;;                    (f 2)))) (lambda (x) ((lambda (y) y) 0 )))
