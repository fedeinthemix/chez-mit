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

;; Use of gensym '#:' syntax.
#!chezscheme

(library (mit core)
  (export declare usual-integrations integrate-operator integrate
          error assert warn ignore-errors bkpt
          guarantee-symbol
          exact-rational? exact-positive-integer? exact-integer?
          exact-nonnegative-integer?
          guarantee-exact-integer
          guarantee-exact-positive-integer guarantee-exact-nonnegative-integer
          load*
          start-canonicalizing-symbols!
          start-preserving-case!
          default-object?
          pp print
          define-integrable
          true false unspecific
          runtime
          there-exists? for-all?
          symbol<? generate-uninterned-symbol string->uninterned-symbol
          undefined-value?
          string-find-next-char-in-set string-search-forward string-head
          string-tail
          write-line
          symbol-append
          symbol
          sort
          delq delv
          run-shell-command user-homedir-pathname system-tmpdir-pathname
          ->namestring
          graphics-type graphics-type-name)
  (import (rename (except (rnrs) error assert) (remq delq) (remv delv))
          (prefix (only (rnrs) assert) r6rs:)
          ;;(except (chezscheme) error)
          (only (chezscheme) load eval eval-when case-sensitive void pretty-print
                warning format void
                current-time time-second time-nanosecond
                gensym 1+ getenv system break)
          (prefix (only (chezscheme) error sort) chez:)
          (only (srfi :13) string-index string-contains)
          (except (mit arity) procedure-arity) ; use version in apply-hook
          (mit curry)
          (mit arithmetic))

;; Declare is used at the start of files.  According to R6RS a library
;; must start with definitions.  (define (declare args) (if #f #f))
(define-syntax declare
  (syntax-rules ()
    ((_ args ...) (define #:declare args ...))))

(define (usual-integrations . args) #f)

(define-syntax integrate-operator
  (syntax-rules ()
    ((_ args ...) unspecific)))

(define-syntax integrate
  (syntax-rules ()
    ((_ args ...) unspecific)))

(define (load* f env)
  (load (string-append f ".scm")
        (lambda (x)
          (eval x env))))

(eval-when (compile eval load)
  (define (start-canonicalizing-symbols!)
    (case-sensitive #f))

  (define (start-preserving-case!)
    (case-sensitive #t)))

(define (default-object? x)
  (eq? (void) x))

(define-syntax define-integrable
  (syntax-rules ()
    ((_ form body ...) (define form body ...))))

(define* (pp object #:optional (port (current-output-port)) (display? #f))
  (pretty-print object port))

(define print pp)

(define* (error msg #:optional (irritant 'not-specified) . rest)
  (apply chez:error 'not-specified msg irritant rest))

(define* (warn msg #:optional (irritant 'not-specified) . rest)
  (apply warning 'warn msg irritant rest))

(define-syntax assert
  (syntax-rules ()
    ((_ form rest ...) (r6rs:assert form))))

(define* (ignore-errors thunk #:optional map-error)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
       (lambda (x)
         (cond ((or (default-object? map-error)
                    (not map-error))
                (if (error? x) (k x) x))
               ((and (procedure? map-error)
                     (procedure-arity-valid? map-error 1))
                (lambda (condition)
                  (k (map-error condition))))
               (else
                (error "wrong-type-argument" map-error
                       "map-error procedure"
                       'IGNORE-ERRORS))))
              thunk))))

(define (bkpt datum . arguments)
  (break 'bkpt datum arguments))

(define (guarantee-symbol x msg)
  (if (not (symbol? x))
      (error msg x "not a symbol")))

(define (exact-rational? x)
  (and (rational? x)
       (exact? x)))

(define (exact-positive-integer? x)
  (and (integer? x)
       (positive? x)
       (exact? x)))

(define (exact-integer? x)
  (and (integer? x)
       (exact? x)))

(define (exact-nonnegative-integer? x)
  (and (integer? x)
       (not (negative? x))
       (exact? x)))

(define (guarantee-exact-integer x msg)
  (if (not (exact-integer? x))
      (error msg x "not an exact integer")))

(define (guarantee-exact-positive-integer x msg)
  (if (not (exact-positive-integer? x))
      (error msg x "not an exact positive integer")))

(define (guarantee-exact-nonnegative-integer x msg)
  (unless (or (zero? x) (exact-positive-integer? x))
      (error msg x "not an exact positive integer")))

(define true #t)
(define false #f)

(define unspecific (void))

(define (runtime)
  (let ((t (current-time 'time-process)))
    (+ (time-second t)
       (/ (time-nanosecond t) 1e9))))

(define (there-exists? items predicate)
  (let loop ((items* items))
    (if (pair? items*)
        (if (predicate (car items*))
            #t
            (loop (cdr items*)))
        (begin
          (if (not (null? items*))
              (error ":not-list items" 'THERE-EXISTS?))
          #f))))

(define (for-all? items predicate)
  (let loop ((items* items))
    (if (pair? items*)
        (if (predicate (car items*))
            (loop (cdr items*))
            #f)
        (begin
          (if (not (null? items*))
              (error ":not-list items" 'FOR-ALL?))
          #t))))

(define (symbol<? x y)
  (string<? (symbol->string x)
            (symbol->string x)))

(define* (generate-uninterned-symbol #:optional s)
  (if (default-object? s)
      (gensym)
      (if (symbol? s) 
          (gensym (symbol->string s))
          (gensym s))))

(define string->uninterned-symbol gensym)

(define (undefined-value? object)
  (or (eq? object unspecific)
      ;;(and (variable? object) (not (variable-bound? object)))
      ;;(eq? object (object-new-type (ucode-type constant) 2))
      ))

(define string-find-next-char-in-set string-index)

(define (string-search-forward pattern string)
  (string-contains string pattern))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))

(define write-line
  (case-lambda
    ((obj) (write-line obj (current-output-port)))
    ((obj port) (display obj port) (newline port))))

(define symbol-append
  (lambda args
    (string->symbol (apply string-append (map symbol->string args)))))

(define (symbol . args)
  (define (ensure-symbol s)
    (cond
     ((symbol? s) s)
     ((number? s) (string->symbol (number->string s)))
     ((string? s) (string->symbol s))
     (else (error "wrong type" s 'symbol))))
  (apply symbol-append (map ensure-symbol args)))

(define (sort elements proc)
  (chez:sort proc elements))

(define (run-shell-command cmd . rest)
  (system cmd))

(define (user-homedir-pathname)
  (string-append (getenv "HOME") "/"))

(define (system-tmpdir-pathname) "/tmp/")

(define (->namestring pathname)
  pathname)

;;; temporary graphics fix
(define (graphics-type arg) #f)

;; This function is used in a context where symbols are canonicalized.
;; Therefore we emit a lowercase symbol.  (This module is loaded
;; preserving case.)
(define (graphics-type-name name) 'x)

)
