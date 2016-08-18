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

;; R6RS doesn't allow 1+, 1-, ...
#!chezscheme

(library (mit arithmetic)
  (export fix:+ fix:- fix:= fix:> fix:< fix:remainder fix:quotient
          fix:xor index-fixnum?
          fix:<= fix:>= fix:* largest-fixnum fix:zero? fix:negative?
          fix:1+ fix:-1+
          fix:fixnum? fix:lsh
          int:+ int:- int:* int:quotient int:< int:> int:= int:zero?
          int:negate int:->flonum
          flo:= flo:< flo:> flo:- flo:+ flo:* flo:/ ;flo:flonum?
          flo:atan2 flo:log flo:abs flo:exp flo:sin flo:cos flo:tan
          flo:asin flo:acos flo:atan flo:sqrt flo:expt flo:floor flo:zero?
          flo:random-unit flo:negate flo:truncate->exact
          -1+ 1+ 1-
          ceiling->exact floor->exact round->exact truncate->exact
          integer-divide integer-divide-quotient integer-divide-remainder
          conjugate
          rationalize->exact
          real:*)
  (import (rnrs)
          (rnrs r5rs)
          (only (chezscheme) 1+ 1- random))

(define fix:+ fx+)
(define fix:- fx-)
(define fix:= fx=?)
(define fix:< fx<?)
(define fix:<= fx<=?)
(define fix:> fx>?)
(define fix:>= fx>=?)
(define fix:* fx*)
(define fix:quotient quotient)
(define fix:remainder fxmod)
(define fix:xor fxxor)
(define fix:zero? fxzero?)
(define fix:negative? fxnegative?)
(define (fix:-1+ x) (fix:+ -1 x))
;;(define fix:1+ 1+)
(define (fix:1+ x) (fix:+ 1 x))
(define fix:fixnum? fixnum?)

(define index-fixnum? fixnum?)
(define largest-fixnum greatest-fixnum)

(define fix:lsh fxarithmetic-shift)

(define int:+ fx+)
(define int:- fx-)
(define int:* fx*)
(define int:quotient fxdiv)
(define int:< fx<?)
(define int:> fx>?)
(define int:= fx=?)
(define int:zero? fxzero?)
(define (int:negate x) (int:- x))

(define int:->flonum inexact)

(define flo:= fl=?)
(define flo:< fl<?)
(define flo:> fl>?)
(define flo:- fl-)
(define flo:+ fl+)
(define flo:* fl*)
(define flo:/ fl/)
(define (flo:negate x) (fl- x))
(define (flo:truncate->exact x) (exact (truncate x)))

;;(define (flo:flonum? obj) (or (flonum? obj) (flo:vector? obj)))
(define flo:atan2 flatan)
(define flo:log fllog)
(define flo:abs  flabs)
(define flo:exp  flexp)
(define flo:sin  flsin)
(define flo:cos  flcos)
(define flo:tan  fltan)
(define flo:asin  flasin)
(define flo:acos  flacos)
(define flo:atan  flatan)
(define flo:sqrt  flsqrt)
(define flo:expt  flexpt)

(define flo:floor flfloor)

(define flo:zero? flzero?)

(define (flo:random-unit state)
  (random 1.0))

(define (-1+ num) (+ -1 num))

(define (ceiling->exact number)
  (inexact->exact (ceiling number)))

(define (floor->exact number)
  (inexact->exact (floor number)))

(define (round->exact number)
  (inexact->exact (round number)))

(define (truncate->exact number)
  (inexact->exact (truncate number)))

(define (integer-divide x y)
  (cons (quotient x y)
        (remainder x y)))

(define integer-divide-quotient car)

(define integer-divide-remainder cdr)

(define (conjugate z)
  (cond ((complex? z)
         (make-rectangular (real-part z)
                           (- (imag-part z))))
        ((real? z)
         z)
        (else
         (error 'CONJUGATE "wrong-type-argument" z #f))))

(define (rationalize->exact r1 r2)
  (rationalize (exact r1) (exact r2)))

(define (real:* x y)
  (unless (and (real? x) (real? y))
    (error 'real:* "Invalid arguments" x y))
  (* x y))

)
