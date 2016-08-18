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
(library (mit streams)
  (export head tail empty-stream? stream-head stream-tail
          prime-numbers-stream the-empty-stream)
  (import (except (rnrs) error assert)
          (rnrs r5rs)
          (mit core)
          (mit arithmetic)
          (rename (srfi :41) (stream-cons cons-stream)
                             (stream-fold stream-accumulate)))

(define head stream-car)

(define tail stream-cdr)

(define the-empty-stream stream-null)

(define empty-stream? stream-null?)

(define (stream-head x n)
  (if (zero? n)
      '()
      (cons (stream-car x)
            (stream-head (stream-cdr x) (- n 1)))))

(define (stream-tail stream index)
  (guarantee-exact-nonnegative-integer index 'STREAM-TAIL)
  (let loop ((stream stream) (index index))
    (if (> index 0)
        (begin
          (if (not (stream-pair? stream))
              (error ":bad-range-argument" index 'STREAM-TAIL))
          (loop (stream-cdr stream) (- index 1)))
        stream)))

(define (square x) (* x x))

(define prime-numbers-stream
  (cons-stream
   2
   (letrec
       ((primes (cons-stream 3 (fixnum-filter 5)))
        (fixnum-filter
         (let ((limit (fix:- (largest-fixnum) 2)))
           (lambda (n)
             (if (fix:<= n limit)
                 (let loop ((ps primes))
                   (cond ((fix:< n (fix:* (stream-car ps) (stream-car ps)))
                          (cons-stream n (fixnum-filter (fix:+ n 2))))
                         ((fix:= 0 (fix:remainder n (stream-car ps)))
                          (fixnum-filter (fix:+ n 2)))
                         (else
                          (loop (stream-cdr ps)))))
                 (generic-filter n)))))
        (generic-filter
         (lambda (n)
           (let loop ((ps primes))
             (cond ((< n (square (stream-car ps)))
                    (cons-stream n (generic-filter (+ n 2))))
                   ((= 0 (remainder n (stream-car ps)))
                    (generic-filter (+ n 2)))
                   (else
                    (loop (stream-cdr ps))))))))
     primes)))

)
