#!r6rs
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
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
(library (mit environment)
  (export ge access
          user-initial-environment
          system-global-environment
          environment-define
          extend-top-level-environment
          environment-bindings
          nearest-repl/environment
          environment-bound?
          environment-assign!
          environment-lookup
          environment-link-name
          environment-update-from-child)
  (import (except (rnrs) error assert)
          (only (chezscheme) interaction-environment set-top-level-value!
                scheme-environment copy-environment environment-symbols
                define-top-level-value
                top-level-value top-level-bound?
                top-level-syntax? define-top-level-syntax top-level-syntax))

(define user-initial-environment
  (copy-environment (scheme-environment))
  ;;(interaction-environment)
  )

(define system-global-environment
  (copy-environment (scheme-environment)))

(define (ge env)
  (interaction-environment env))

;; XXX: should add the set! use. (identifier-syntax?)
(define-syntax access
  (syntax-rules ()
    ((_ name env) (top-level-value 'name env))))

(define (environment-define env var obj)
  (define-top-level-value var obj env))

(define (extend-top-level-environment env)
  (copy-environment env))

(define (environment-bindings env)
  (let ((ss (if env
                (filter (lambda (s) (top-level-bound? s env))
                        (environment-symbols env))
                '())))
    (map (lambda (s) (list s (top-level-value s env))) ss)))

(define nearest-repl/environment
  (lambda () (interaction-environment)))

(define (environment-bound? env symbol)
  (top-level-bound? symbol env))

(define (environment-assign! env symbol object)
  (set-top-level-value! symbol object env))

(define (environment-lookup env symbol)
  (top-level-value symbol env))

(define (environment-link-name dest-env src-env name)
  (define-top-level-value name (top-level-value name src-env) dest-env))

(define environment-update-from-child
  (case-lambda
    ((env child-env)
     (environment-update-from-child env child-env (environment-symbols child-env)))
    ((env child-env child-symbols)
     (for-each (lambda (s)
                 (unless (top-level-bound? s env)
                   (cond
                    ((top-level-syntax? s child-env)
                     (define-top-level-syntax s (top-level-syntax s child-env) env))
                    (else
                     (define-top-level-value s (top-level-value s child-env) env)))))
               child-symbols))))

)
