#!r6rs
;;; Copyright © 2016 Federico Beffa
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code
(library (mit curry)
  (export lambda* define*)
  (import (rnrs)
          (only (chezscheme) gensym? enumerate syntax-error list-head
                last-pair void))

;; allow #:optional, #:rest and . formals.
;; ((lambda* (a #:optional (b (+ a 1)) #:rest c) (cons* a b c)) 1)
;; => (1 2)
(define-syntax lambda*
  (lambda (x)
    (define (keyword-id? kn p)
      (if (and (pair? p) (identifier? (car p))
               (gensym? (syntax->datum (car p)))
               (equal? (symbol->string (syntax->datum (car p))) kn))
          #t #f))
    (define (keyword-id-index kn ls)
      (let ((element (find (lambda (p) (keyword-id? kn p))
                           (map (lambda (e i) (cons e i)) 
                                ls (enumerate ls)))))
        (if element (cdr element) #f)))
    ;; return 3 lists with required, optional and rest formal
    ;; parameter identifiers.
    (define (split-formals ls)
      (let ((opt-idx (keyword-id-index "optional" ls))
            (rest-idx (keyword-id-index "rest" ls)))
        (cond
         ((and opt-idx rest-idx)
          (when (< rest-idx opt-idx)
            (syntax-error 'lambda* "#:optional keyword must precede #:rest one"))
          (let ((o+r (list-tail ls (+ 1 opt-idx))))
            (values (list-head ls opt-idx)
                    (list-head o+r (- rest-idx (+ 1 opt-idx)))
                    (list-tail o+r (- (+ 1 rest-idx) (+ 1 opt-idx))))))
         ((and (not opt-idx) (not rest-idx))
          (values ls '() '()))
         ((and opt-idx (not rest-idx))
          (values (list-head ls opt-idx)
                  (list-tail ls (+ 1 opt-idx))
                  '()))
         ((and (not opt-idx) rest-idx)
          (values (list-head ls rest-idx)
                  '()
                  (list-tail ls (+ 1 rest-idx))))
         (else
          (syntax-error 'lambda* "wrong formals format")))))
    (define (last ls)
      (car (last-pair ls)))
    (define (remove-last ls)
      (list-head ls (- (length ls) 1)))
    ;; add the a value to the list of let bindings used to hold the
    ;; default values of optional formal parameters.
    (define (add-default fl d-vals)
      (syntax-case fl ()
        (i (identifier? #'i) (append (list #`(i #,(list #'void))) d-vals))
        ((i v) (identifier? #'i) (append (list #'(i v)) d-vals))))
    ;; extract the identifier of an optional formal parameter
    (define (formals-identifiers fls)
      (map (lambda (fl)
             (syntax-case fl ()
               (i (identifier? #'i) #'i)
               ((i v) (identifier? #'i) #'i)))
           fls))
    ;; returns the default value of an optional formal parameter
    (define (formals-defaults fls)
      (map (lambda (fl)
             (syntax-case fl ()
               (i (identifier? #'i) (list #'void))
               ((i v) (identifier? #'i) #'v)))
           fls))
    ;; return the 'rest' formal parameter defined in one of the two
    ;; possible ways: using the #:rest keyword, or the '.' notation.
    (define (rest-formal rest-fl dot-fl)
      (when (and (not (null? rest-fl)) (not (null? dot-fl)))
        (syntax-error 'lambda* "#:rest keyword not allowed with . notation"))
      (when (> (length rest-fl) 1)
        (syntax-error 'lambda* "multiple #:rest or . formals"))
      (cond
       ((not (null? rest-fl)) (car rest-fl))
       ((not (null? dot-fl)) dot-fl)
       (else '())))
    (syntax-case x ()
      ((k (f ... . r) b1 b2 ...)
       (let ((formals #'(f ...))
             (dot-fl #'r))  ; when bound a synbol, when not bound '()
         (let-values (((req-fls opt-fls rest-fl) (split-formals formals)))
           ;; rest-fl is a one (or more, we check the length later)
           ;; element list when bound and the empty list when not
           ;; bound
           #`(case-lambda
               ((#,@req-fls #,@(formals-identifiers opt-fls) . #,(rest-formal rest-fl dot-fl))
                b1 b2 ...)
               #,@(let loop ((o-fls opt-fls)
                             (d-vals (if (null? (rest-formal rest-fl dot-fl))
                                         '()
                                         #`((#,(rest-formal rest-fl dot-fl) '())))))
                    (if (null? o-fls)
                        #`(((#,@req-fls)
                            (let #,d-vals
                              b1 b2 ...)))
                        #`(((#,@req-fls #,@(formals-identifiers o-fls))
                            (let #,d-vals
                              b1 b2 ...))
                           #,@(loop (remove-last o-fls)
                                    (add-default (last o-fls) d-vals))))))))))))

;; Allows curried definitions with optional parameters of the form:
;; (define* (((fbe a) b) #:optional (c 3)) (list a b c))
;; (((fbe 1) 2)) => (1 2 3)
(define-syntax define*
  (lambda (x)
    (syntax-case x ()
      ((_ v e) (identifier? #'v) #'(define v e))
      ((_ formals b1 b2 ...)
       (let loop ((fls #'formals) (body #'(b1 b2 ...)))
         (let ((fls-datum (syntax->datum fls)))
           (if (and (pair? fls-datum) (symbol? (car fls-datum)))
               (with-syntax (((var . fls) fls))
                 #`(define var (lambda* fls #,@body)))
               (syntax-case fls ()
                 ((fs-a . fs-b)
                  (loop #'fs-a
                        #`((lambda* fs-b #,@body))))))))))))

)
