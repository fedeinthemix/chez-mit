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

(library (mit apply-hook)
  (export make-apply-hook apply-hook? apply-hook-procedure set-apply-hook-procedure!
          apply-hook-extra set-apply-hook-extra! apply-hook-arity
          ;; We export the procedure 'procedure-arity' which is also
          ;; exported by the library '(mit arity)'.  The difference to
          ;; the latter is that this one supports 'apply-hooks'.
          ;; Thus, of you make use of 'apply-hooks' you want to use
          ;; this one.  Otherwise use the other one.
          procedure-arity)
  (import (rnrs)
          (rename (mit arity) (procedure-arity arity:procedure-arity)))

(define-record-type apply-hook-symbol (fields >symbol))

(define-record-type (apply-hook %make-apply-hook %apply-hook?)
  (fields (mutable procedure %apply-hook-procedure %set-apply-hook-procedure!)
          (mutable extra %apply-hook-extra %set-apply-hook-extra!)))

(define (make-apply-hook proc extra)
  ;;(guarantee-procedure proc)
  (let ((ahook (%make-apply-hook proc extra)))
    (define (dispatch msg . args)
      (if (apply-hook-symbol? msg)
          (case (apply-hook-symbol->symbol msg)
            ((get-hook) ahook) ; used by 'apply-hook?'.
            ((get-proc) (%apply-hook-procedure ahook))
            ((set-proc!) (apply %set-apply-hook-procedure! ahook args))
            ((get-extra) (%apply-hook-extra ahook))
            ((set-extra!) (apply %set-apply-hook-extra! ahook args))
            ((arity) (arity:procedure-arity (%apply-hook-procedure ahook)))
            (else
             (error 'make-apply-hook/dispatch "Undefined message symbol" msg)))
          (begin
            ;; (guarantee-procedure-of-arity (%apply-hook-procedure ahook)
            ;;                               (+ 1 (length args))
            ;;                               'make-apply-hook/dispatch)
            (apply (%apply-hook-procedure ahook) msg args)
            ;; (let ((p (%apply-hook-procedure ahook)))
            ;;   (if (procedure? p) (apply p msg args)))
            )))
    dispatch))

(define (apply-hook? ah)
  (call/cc
   (lambda (k)
     (with-exception-handler
         (lambda (x) (k #f))
       (lambda ()
         (%apply-hook? (ah (make-apply-hook-symbol 'get-hook))))))))

(define (apply-hook-procedure ah) (ah (make-apply-hook-symbol 'get-proc)))
(define (set-apply-hook-procedure! ah proc)
  (guarantee-procedure proc)
  (ah (make-apply-hook-symbol 'set-proc!) proc))

(define (apply-hook-extra ah) (ah (make-apply-hook-symbol 'get-extra)))
(define (set-apply-hook-extra! ah extra)
  (ah (make-apply-hook-symbol 'set-extra!) extra))

(define (apply-hook-arity ah) (ah (make-apply-hook-symbol 'arity)))

(define (procedure-arity proc)
  (guarantee-procedure proc)
  (if (apply-hook? proc)
      (apply-hook-arity proc)
      (arity:procedure-arity proc)))

)
