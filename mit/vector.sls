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

(library (mit vector)
  (export make-initialized-vector flo:flonum?
          flo:vector-cons flo:vector-length flo:vector-ref
          flo:vector-set! flo:vector? flo:subvector
          subvector vector-head
          subvector-move-left! subvector-fill!
          guarantee-vector guarantee-subvector guarantee-subvector-range
          vector-tail)
  (import (except (rnrs) error assert)
          (prefix (only (rnrs) error) chez:)
          (mit core)
          (mit arithmetic))

(define (make-initialized-vector length initialization)
  ;; LENGTH is checked by MAKE-VECTOR
  (let ((vector (make-vector length)))
    (let loop ((index 0))
      (if (fix:< index length)
          (begin
            (vector-set! vector index (initialization index))
            (loop (fix:+ index 1)))))
    vector))

(define (flo:flonum? obj) (or (flonum? obj) (flo:vector? obj)))

(define flo:vector-cons make-vector)

(define flo:vector-length vector-length)

(define flo:vector-ref vector-ref)

(define flo:vector-set! vector-set!)

(define flo:vector? vector?)

(define (flo:subvector vector start end)
  (let* ((idx-end (- end start))
         (sv (flo:vector-cons idx-end)))
    (let loop ((i 0))
      (when (< i idx-end)
        (flo:vector-set! sv i (flo:vector-ref vector (+ start i)))
        (loop (+ 1 i))))
    sv))

(define (subvector vector start end)
  (let* ((idx-end (- end start))
         (sv (make-vector idx-end)))
    (let loop ((i 0))
      (when (< i idx-end)
        (vector-set! sv i (vector-ref vector (+ start i)))
        (loop (+ 1 i))))
    sv))

(define (vector-head vector end)
  (subvector vector 0 end))

(define (subvector-move-left! v1 start1 end1 v2 start2)
  (guarantee-exact-nonnegative-integer start1 'subvector-move-left!)
  (guarantee-exact-nonnegative-integer start2 'subvector-move-left!)
  (guarantee-exact-nonnegative-integer end1 'subvector-move-left!)
  (guarantee-vector v1 'subvector-move-left!)
  (guarantee-vector v2 'subvector-move-left!)
  (unless (and (<= (vector-length v1) end1)
               (<= (- end1 start1) (- (vector-length v2) start2)))
    (error 'subvector-move-left! "Invalid index" start1 end1 start2))
  (let loop ((i 0))
    (when (< i (- end1 start1))
      (vector-set! v2 (+ start2 i) (vector-ref v1 (+ start1 i)))
      (loop (+ i 1)))))

(define (subvector-fill! vector start end value)
  (guarantee-subvector vector start end 'SUBVECTOR-FILL!)
  (do ((i start (+ i 1)))
      ((= i end) unspecific)
    (vector-set! vector i value)))

(define (guarantee-vector object procedure)
  (if (not (vector? object))
      (chez:error procedure "Not a vector" object)))

(define (guarantee-subvector v s e procedure)
  (guarantee-vector v procedure)
  (guarantee-exact-nonnegative-integer s procedure)
  (guarantee-exact-nonnegative-integer e procedure)
  (guarantee-subvector-range v s e procedure))

(define (guarantee-subvector-range v s e procedure)
  (if (not (fix:<= s e))
      (error ":bad-range-argument" s procedure))
  (if (not (fix:<= e (vector-length v)))
      (error ":bad-range-argument" e procedure)))

(define (vector-tail vector start)
  (guarantee-vector vector 'VECTOR-TAIL)
  (subvector vector start (vector-length vector)))

)
