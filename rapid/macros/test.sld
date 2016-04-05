;;; Rapid Macros --- An implementation of Oleg Kiselyov's CK macros

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-library (rapid macros test)
  (export run-tests)
  (import (scheme base) (rapid macros) (rapid test))
  (begin
    (define (run-tests)

      (test-begin "Rapid Macros")

      (test-begin "define-macro")

      (test-equal "Macros work in definition contexts"
		  10
		  (let ()
		    (define-macro m ()
		      ((m 'a) 'a))
		    (m '(define x 10))
		    x))

      (test-equal "Macros allow pattern matching"
		  11
		  (let ()
		    (define-macro m (=>)
		      ((m => 'a ...) '(a ...)))
		    (m => 'define 'x '11)
		    x))

      (test-equal "The ellipsis can be specified"
		  12
		  (let ()
		    (define-macro m ::: ()
		      ((m 'a :::) '(a :::)))
		    (m 'define 'x '12)
		    x))
      
      (test-end "define-macro")

      (test-begin "m-expression")

      (test-equal "Macros can be used in expression contexts"
		  13
		  (let ()
		    (define-macro m ()
		      ((m 'a) 'a))
		    (+ (m-expression (m '12)) 1)))
      
      (test-end "m-expression")

      (test-begin "m-quote")

      (test-equal "Quote allows the expansion into quoted values"
		  (list 3 '(+ 1 2))
		  (let ()
		    (define-macro m ()
		      ((m) '(+ 1 2)))
		    (list (m-expression (m))
			  (m-expression (m-quote (m))))))		    
      
      (test-end "m-quote")

      (test-begin "m-cons")

      (test-equal "Syntactic pairs can be created"
		  '(1 . 2)
		  (m-expression
		   (m-quote (m-cons '1 '2))))
      
      (test-end "m-cons")

      (test-begin "m-list")

      (test-equal "Syntactic lists are also possible"
		  3
		  (m-expression (m-list '+ '1 '2)))
      
      (test-end)
      
      (test-begin "m-quasiquote")

      (test-equal "Quasiquotation is also possible on the macro level"
		  3
		  (m-expression `(+ . ,(m-list '1 '2))))
      
      (test-equal "Unquote-splicing is supported as well"
		  3
		  (m-expression `(+ ,@(m-list '1 '2))))
      
      (test-end "m-quasiquote")
      
      (test-begin "m-shift")

      (test-equal "A limited version of delimited continuations is available"
		  2
		  (m-expression
		   (m-shift
		    k
		    (define-syntax m
		      (syntax-rules ()
			((m) (k '2))))
		    (m))))
			    	   
      (test-end "m-shift")

      (test-begin "m-if")

      (test-equal "Conditionals on the macro level"
		  '(5 6)
		  (m-expression
		   `(list (,(m-if '#t '+ '*) '2 '3)
			  (,(m-if '#f '+ '*) '2 '3))))
      
      (test-end "m-if")

      (test-begin "m-let")

      (test-equal "Named let on the macro level"
		  '(3 2 1)
		  (m-expression
		   (m-quote
		    (m-let m-loop (('x '(1 2 3)) ('y '()))
			   (m-if (m-null? 'x)
				 'y
				 (m-loop (m-cdr 'x) (m-cons (m-car 'x) 'y)))))))
      
      (test-end "m-let")

      (test-begin "m-symbol?")

      (test-equal "Identifiers are symbols"
		  #t
		  (m-expression
		   (m-if (m-symbol? 'a) '#t '#f)))

      (test-equal "Numbers aren't symbols"
		  #f
		  (m-expression
		   (m-if (m-symbol? '34) '#t '#f)))

      (test-equal "The empty list is no symbol"
		  #f
		  (m-expression
		   (m-if (m-symbol? '()) '#t '#f)))

      (test-end "m-symbol?")
      
      ;; TODO: m-eq?, m-eqv?, m-gensym
      
      ;; TODO: m-error
      
      (test-end "Rapid Macros")

      #t)))
