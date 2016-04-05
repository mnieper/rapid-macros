(import (scheme base)
	(scheme write)
	(srfi 2)
	(rapid macros))

(define-macro simple-match ()
  ((simple-match expr (pattern template) ...)
   (m-expression
     `(call-with-current-continuation
       (lambda (return)
         (let ((e expr))
	   (or (and-let* ,(%compile-pattern 'pattern 'e) (return template))
	       ...
	       (error "does not match" expr))))))))

(define-macro %compile-pattern ()
  ((%compile-pattern '() 'e)
   '(((null? e))))
  ((%compile-pattern '(pattern1 pattern2 ...) 'e)
   `(((not (null? e)))
     (e1 (car e))
     (e2 (cdr e))
     ,@(%compile-pattern 'pattern1 'e1)
     ,@(%compile-pattern '(pattern2 ...) 'e2)))
  ((%compile-pattern 'x 'e)
   (m-if (m-symbol? 'x)
	 '((x e))
	 '(((equal? x e))))))

(define (f e)
  (simple-match e
	        (10 'ten)
		((11 x) x)))

(display (f 10))
(newline)

(display (f '(11 eleven)))
(newline)
