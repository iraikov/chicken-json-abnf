;;
;; 
;;  A parser for JavaScript Object Notation (JSON) 
;;
;;  Based on RFC 4627, "The application/json Media Type for JavaScript
;;  Object Notation (JSON)" 
;;
;;
;;   Copyright 2009-2018 Ivan Raikov
;;
;;
;;   This program is free software: you can redistribute it and/or
;;   modify it under the terms of the GNU General Public License as
;;   published by the Free Software Foundation, either version 3 of
;;   the License, or (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;;
;;   A full copy of the GPL license can be found at
;;   <http://www.gnu.org/licenses/>.


(module json-abnf

	(parser)

	(import scheme (chicken base) srfi-1
                utf8 utf8-srfi-14
                (prefix abnf abnf:) 
		(prefix abnf-consumers abnf:) 
		)

;; helper macro for mutually-recursive parser definitions

(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

;; construct numbers from consumed chars
(define consumed-chars->number
  (abnf:consumed-chars->list 
   (compose string->number list->string)))

;; shortcut for (bind consumed-chars->number ... )
(define-syntax bind-consumed->number
  (syntax-rules () 
    ((_ p)    (abnf:bind consumed-chars->number p))
    ))


(define escaped-char->control-char
  (abnf:consumed-chars->list 
   (lambda (x) 
     (case (car x)
       ((#\b)    #\backspace )
       ((#\f)    #\page )
       ((#\n)    #\newline )
       ((#\r)    #\return )
       ((#\t)    #\tab )
       (else     (car x))))))
			  

(define-syntax bind-consumed->control-char
  (syntax-rules () 
    ((_ p)    (abnf:bind escaped-char->control-char p))
    ))


(define consumed-chars->char-code
  (abnf:consumed-chars->list 
   (compose (lambda (x) (integer->char (string->number x 16)))
	    list->string)))


(define-syntax bind-consumed->char-code
  (syntax-rules () 
    ((_ p)    (abnf:bind consumed-chars->char-code p))
    ))

(define (value?  x)    (or (string? x) (number? x) (boolean? x)
			   (vector? x) (null? x) (pair? x) (symbol? x)))

(define consumed-values (abnf:consumed-objects value?))
(define consumed-values->list
  (abnf:consumed-objects-lift consumed-values))

;; shortcut for (abnf:bind (consumed-values->list ...) ... )
(define-syntax bind-consumed-values->list
  (syntax-rules () 
    ((_ l p)    (abnf:bind (consumed-values->list l)  p))
    ((_ p)      (abnf:bind (consumed-values->list)    p))
    ))

;; construct vectors from consumed values
(define consumed-values->vector 
  ((abnf:consumed-objects-lift consumed-values)
   list->vector))

;; shortcut for (abnf:bind (consumed-values->vector ...) ... )
(define-syntax bind-consumed-values->vector
  (syntax-rules () 
    ((_ p)      (abnf:bind consumed-values->vector  p))
    ))

;; construct pairs from consumed values
(define consumed-values->pair 
  ((abnf:consumed-objects-lift consumed-values)
   (lambda (l)
     (or (and (null? l) l) 
	 (cons (car l) (cadr l))))
   ))

;; shortcut for (abnf:bind (consumed-values->pair ...) ... )
(define-syntax bind-consumed-values->pair
  (syntax-rules () 
    ((_ p)      (abnf:bind consumed-values->pair  p))
    ))

(define ws (abnf:repetition (abnf:set-from-string " \t\r\n")))

(define (structural-char c)
  (abnf:concatenation ws (abnf:char c) ws))

(define begin-array     (structural-char #\[))
(define begin-object    (structural-char #\{))
(define end-array       (structural-char #\]))
(define end-object      (structural-char #\}))
(define name-separator  (abnf:drop-consumed (structural-char #\:)) )
(define value-separator (abnf:drop-consumed (structural-char #\,)) )

(define value 
  (vac
   (abnf:alternatives 
    false null true number p-string object array)))

(define false
  (abnf:bind
   (lambda x (list #f))
   (abnf:lit "false")))

(define null
  (abnf:bind
   (lambda x (list 'null))
   (abnf:lit "null")))

(define true
  (abnf:bind
   (lambda x (list #t))
   (abnf:lit "true")))

(define escaped 
  (abnf:concatenation
   (abnf:drop-consumed (abnf:char #\\))
   (abnf:alternatives
    (bind-consumed->control-char
     (abnf:set  (char-set #\" #\\ #\/ #\b #\f #\n #\r #\t)))
    (abnf:concatenation 
     (abnf:drop-consumed (abnf:char #\u))
     (bind-consumed->char-code
      (abnf:repetition-n 4 abnf:hexadecimal)))
    )))

     
(define char
  (abnf:alternatives
   (abnf:set 
    (char-set-union
      (ucs-range->char-set #x20 #x22)
      (ucs-range->char-set #x23 #x5C)
      (ucs-range->char-set #x5D #x10FFFF)))
   escaped))



(define p-string
  (abnf:alternatives
   (abnf:bind-consumed->string
    (abnf:concatenation
     (abnf:drop-consumed (abnf:char #\"))
     (abnf:repetition1 char)
     (abnf:drop-consumed (abnf:char #\"))))
   (abnf:bind (lambda (x) (list ""))  (abnf:concatenation (abnf:char #\") (abnf:char #\")))
   ))
   


(define number
  (let* ((digit        (abnf:range #\0 #\9))
	 (digits       (abnf:repetition1 digit))
	 (fraction     (abnf:concatenation (abnf:char #\.) digits))
	 (significand  (abnf:alternatives 
			(abnf:concatenation 
			 digits
			 (abnf:optional-sequence fraction)) 
			fraction))
	 (exp          (abnf:concatenation
			(abnf:set-from-string "eE") 
			(abnf:concatenation
			 (abnf:optional-sequence 
			  (abnf:set-from-string "+-"))
			 digits)))
	 (sign         (abnf:optional-sequence 
			(abnf:char #\-))))

    (bind-consumed->number
     (abnf:concatenation 
      sign
      (abnf:concatenation 
       significand
       (abnf:optional-sequence exp))))))


(define p-member 
  (bind-consumed-values->pair
   (abnf:concatenation
    p-string
    name-separator 
    value
    )))


(define object
  (bind-consumed-values->list 
    (abnf:concatenation
     (abnf:drop-consumed begin-object)
     (abnf:optional-sequence
      (abnf:concatenation
       p-member 
       (abnf:repetition
	(abnf:concatenation 
	 value-separator 
	 p-member))))
     (abnf:drop-consumed end-object))))


(define array
  (bind-consumed-values->vector
   (abnf:concatenation
    (abnf:drop-consumed begin-array)
    (abnf:optional-sequence
     (abnf:concatenation
      value
      (abnf:repetition
       (abnf:concatenation
	value-separator
	value ) )))
    (abnf:drop-consumed end-array)
    )))



(define JSON-text
  (abnf:alternatives object array))


(define (->char-list s)
  (if (string? s) (string->list s) s))


(define (err s)
  (print "JSON parser error on stream: " s)
  `(error))


(define parser
  (lambda (s)
    (JSON-text caar err `(() ,(->char-list s)))))

)
