(let ((h (lambda (b) 
	   (let ((g (lambda (z) z)))
	     (let ((f (lambda (k)
			(if b
			    (k 1)
			    (k 2)))))
	       (let ((y (f (lambda (x) x))))
		 (g y)))))))
  (let ((x (h #t)) (y (h #f)))
    y))


