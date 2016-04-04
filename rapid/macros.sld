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

(define-library (rapid macros)
  (export define-macro
	  m-shift
	  m-expression
	  m-let
	  m-if
	  m-quasiquote
	  m-list->vector
	  m-cons
	  m-car
	  m-cdr
	  m-null?
	  m-append
	  m-list
	  m-quote
	  m-list
	  m-eq?
	  m-eqv?
	  m-gensym
	  m-error)
  (import (scheme base))
  (include "macros.scm"))
