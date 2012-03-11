(let ((lp1 '(unspecified)))
 (let ((a
          (set! lp1 (lambda (i x)  (let ((a (= 0 i )))  (if
            a
            x
            (let ((lp2 '(unspecified)))
                (let ((b
                         (set! lp2 (lambda (j f y)  (let ((b (= 0 j )))
                            (if b (lp1 (- i 1 ) y ) (let (($tmp$3 (f y )))  (lp2 (- j 1 ) f $tmp$3 ))))))))
                    (lp2 10 (lambda (n)  (+ n i )) x )))))))))
       (lp1 10 0 )))
