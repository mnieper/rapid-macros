;;; Rapid Macros --- An implementation of Oleg Kiselyov's CK-macros

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

;; Secret literals
(define-syntax :call (syntax-rules ()))
(define-syntax :prepare (syntax-rules ()))

(define-syntax ck
  (syntax-rules (quote quasiquote)
    ((ck () 'v)
     v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (quasiquote va))
     (m-quasiquote-aux :call s va '()))
    ((ck s "arg" (op va ...))
     (op :call s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (quasiquote ea))
     (m-quasiquote-aux :prepare s ea '()))
    ((ck s (op ea ...))
     (op :prepare s ea ... ))))

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro op (literal ...)
       (pattern template)
       ...)
     (define-macro-aux op (... ...) (literal ...) (pattern template) ...))
    ((define-macro op ellipsis (literal ...)
       (pattern template)
       ...)
     (begin
       (define-syntax m
	 (syntax-rules ellipsis ()
	   ((m . args) (define-macro-aux . args))))
       (m op ellipsis (literal ...) (pattern template) ...)))))

(define-syntax define-macro-aux
  (syntax-rules ()
    ((define-macro-aux op ellipsis (literal ...)
       (pattern template)
       ...)
     (begin
       (define-syntax d
	 (syntax-rules ...1 (ellipsis op quote)
	   ((d o e (l ...1) () ((p q r t) ...1))
	    (define-syntax o
	      (syntax-rules e (l ...1 quote :prepare :call)
		((o :prepare s . p)
		 (ck s "arg" (o) . q))
		...1
		((o :prepare s . args)
		 (syntax-error "bad arguments to macro call" (o . args)))
		((o :call s . r) (ck s t))
		...1
		((o . args) (ck () (o . args))))))
	   ((d o e l* (((op . p) t) . pt*) qu*)
	    (d o e l* pt* qu* (p t) () () ()))
	   ((d o e l* pt* (qu ...1) (() t) p q r)
	    (d o e l* pt* (qu ...1 (p q r t))))
	   
	   ((d o e l* pt* qu* (('x ellipsis . p) t) (y ...1) (z ...1) (w ...1))
	    (d o e l* pt* qu* (p t) (y ...1 x e) (z ...1 x e) (w ...1 'x e))) 

	   ((d o e l* pt* qu* (('x . p) t) (y ...1) (z ...1) (w ...1))
	    (d o e l* pt* qu* (p t) (y ...1 x) (z ...1 x) (w ...1 'x)))

	   ((d o e l* pt* qu* ((x ellipsis . p) t) (y ...1) (z ...1) (w ...1))
	    (d o e l* pt* qu* (p t) (y ...1 x e) (z ...1 'x e) (w ...1 'x e)))

	   ((d o e l* pt* qu* ((x . p) t) (y ...1) (z ...1) (w ...1))
	    (d o e l* pt* qu* (p t) (y ...1 x) (z ...1 'x) (w ...1 'x)))))
       (d op ellipsis (literal ...) ((pattern template) ...) ())))))

(define-syntax m-shift
  (syntax-rules (quote :prepare :call)
    ((m-shift :prepare s k body1 body2 ...)
     (ck s "arg" (m-shift) 'k 'body1 'body2 ...))
    ((m-shift :call s 'k 'body1 'body2 ...)
     (begin
       (define-syntax m
	 (syntax-rules ...1 ()
	   ((m)
	    (begin
	      (define-syntax k
		(syntax-rules ...2 ()
		  ((k v) (ck s v))))
	      body1
	      body2
	      ...))))
       (m)))
    ((m-shift k body1 body2 ...)
     (ck () (m-shift k body1 body2 ...)))))

(define-syntax m-expression
  (syntax-rules (quote :prepare :call)
    ((m-expression :prepare s expression)
     (ck s "arg" (m-expression) 'expression))
    ((m-expression :call s 'expression)
     (let ()
       (m-quasiquote (let () (define x ,expression) x))))
    ((m-expression expression)
     (ck () (m-expression expression)))))

(define-macro m-let ()
  ((m-let loop (('variable init) ...) expression)
   (m-shift
    k
    (define-syntax m
      (syntax-rules ()
	((m)
	 (begin
	   (define-macro loop ()
	     ((loop 'variable ...) expression))
	   (k (loop init ...))))))
    (m))))

(define-macro m-cons ()
  ((m-cons 'h 't) '(h . t)))

(define-macro m-car ()
  ((m-car '(h . t)) 'h))

(define-macro m-error ()
  ((m-error 'arg ...) '(syntax-error arg ...)))

(define-macro m-cdr ()
  ((m-cdr '(h . t)) 't)
  ((m-cdr arg) (m-error "not a pair" arg)))

(define-macro m-list->vector ()
  ((m-list->vector '(element ...)) '#(element ...)))

(define-macro m-append ()
  ((m-append) ''())
  ((m-append 'l) 'l)
  ((m-append '() 'l2 'l ...) (m-append 'l2 'l ...))
  ((m-append '(h . t) 'l2 'l ...) (m-cons 'h (m-append 't 'l2 'l ...))))

(define-macro m-quote ()
  ((m-quote 'x) ''x))

(define-macro m-if ()
  ((m-if '#f consequent alternate)
   alternate)
  ((m-if 'test consequent alternate)
   consequent))

(define-macro m-list ()
  ((m-list 'a ...) '(a ...)))

(define-macro m-eq? ()
  ((m-eq? 'id 'v)
   (m-shift
    k
    (define-syntax m
      (syntax-rules ()				       
	((m)
	 (begin
	   (define-syntax id
	     (syntax-rules ()
	       ((id) (k '#f))))
	   (define-syntax ok
	     (syntax-rules ()
	       ((ok) (k '#t))))
	   (define-syntax test
	     (syntax-rules ()
	       ((test v) (id))))
	   (test ok)))))
    (m))))

(define-macro m-eqv? ()
  ((m-eqv? 'id1 'id2)
   (m-shift
    k
    (define-syntax m
      (syntax-rules ()
	((m)
	 (begin
	   (define-syntax test
	     (syntax-rules (id1)
	       ((test id1) (k '#t))
	       ((test x) (k '#f))))
	   (test id2)))))
    (m))))

(define-macro m-null? ()
  ((m-null? '()) '#t)
  ((m-null? 'x) '#f))

(define-macro m-gensym ()
  ((m-gensym)
   (m-shift
    k
    (define-syntax m
      (syntax-rules ()
	((m) (k 'g))))
    (m))))

(define-macro m-quasiquote ()
  ((m-quasiquote form) (m-quasiquote-aux form '())))

(define-macro m-quasiquote-aux (quasiquote unquote unquote-splicing)
  ((m-quasiquote-aux ,form '())
   form) 
  ((m-quasiquote-aux (,@form . rest) '())
   (m-append form (m-quasiquote-aux rest '())))
  ((m-quasiquote-aux `form 'depth)
   (m-list 'quasiquote (m-quasiquote-aux form '(#f . depth))))
  ((m-quasiquote-aux ,form '(#f . depth))
   (m-list 'unquote (m-quasiquote-aux form 'depth)))
  ((m-quasiquote-aux ,@form '(#f . depth))
   (m-list 'unquote-splicing (m-quasiquote-aux '(form . depth))))
  ((m-quasiquote-aux (car . cdr) 'depth)
   (m-cons (m-quasiquote-aux car 'depth) (m-quasiquote-aux cdr 'depth)))
  ((m-quasiquote-aux #(element ...) 'depth)
   (m-list->vector (m-quasiquote-aux (element ...) 'depth)))
  ((m-quasiquote-aux constant 'depth)
   'constant))
