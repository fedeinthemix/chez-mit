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

#!chezscheme

(library (mit list)
  (export reduce-left ;fold-left
          keyword-list?
          get-keyword-value guarantee-alist
          alist? list-of-type? guarantee-list-of-type
          make-initialized-list delq!
          find-matching-item
          sublist
          guarantee-pair except-last-pair)
  (import (except (rnrs) error assert)
          (rnrs mutable-pairs)
          (only (chezscheme) void list-head)
          (only (srfi :1) reduce)
          (mit core)
          (mit curry)
          (mit arithmetic))

(define (reduce-left procedure initial list)
  (reduce (lambda (a b) (procedure b a)) initial list))

;; (define fold-left fold)

(define (keyword-list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
        (and (symbol? (car l1))
             (pair? (cdr l1))
             (not (eq? (cdr l1) l2))
             (loop (cdr (cdr l1)) (cdr l1)))
        (null? l1))))

(define (get-keyword-value klist key)
  (let ((lose (lambda () (error ":not-keyword-list" klist 'GET-KEYWORD-VALUE))))
    (let loop ((klist klist))
      (if (pair? klist)
          (begin
            (if (not (pair? (cdr klist)))
                (lose))
            (if (eq? (car klist) key)
                (car (cdr klist))
                (loop (cdr (cdr klist)))))
          (begin
            (if (not (null? klist))
                (lose))
            (void))))))

(define* (guarantee-alist object #:optional caller)
  (if (not (alist? object))
      (error "not-alist" object caller)))

(define (alist? object)
  (list-of-type? object pair?))

(define (list-of-type? object predicate)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
        (and (predicate (car l1))
             (let ((l1 (cdr l1)))
               (and (not (eq? l1 l2))
                    (if (pair? l1)
                        (and (predicate (car l1))
                             (loop (cdr l1) (cdr l2)))
                        (null? l1)))))
        (null? l1))))

(define* (guarantee-list-of-type object predicate description #:optional caller)
  (if (not (list-of-type? object predicate))
      (error ":wrong-type-argument" object
             description
             (if (default-object? caller) #f caller))))

(define (make-initialized-list length initialization)
  ;;(guarantee-index-fixnum length 'MAKE-INITIALIZED-LIST)
  (let loop ((index (fix:- length 1)) (result '()))
    (if (fix:< index 0)
        result
        (loop (fix:- index 1)
              (cons (initialization index) result)))))

(define (%delete! item items = caller)
  (letrec
      ((trim-initial-segment
        (lambda (items)
          (if (pair? items)
              (if (= item (car items))
                  (trim-initial-segment (cdr items))
                  (begin
                    (locate-initial-segment items (cdr items))
                    items))
              (begin
                (if (not (null? items))
                    (lose))
                '()))))
       (locate-initial-segment
        (lambda (last this)
          (if (pair? this)
              (if (= item (car this))
                  (set-cdr! last
                            (trim-initial-segment (cdr this)))
                  (locate-initial-segment this (cdr this)))
              (if (not (null? this))
                  (error ":not-list" items caller)))))
       (lose
        (lambda ()
          (error ":not-list" items caller))))
    (trim-initial-segment items)))

(define (delq! item items)
  (%delete! item items eq? 'DELQ!))

(define (find-matching-item l pred)
  (if (null? l)
      #f
      (if (pred (car l))
          (car l)
          (find-matching-item (cdr l) pred))))

(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

(define* (guarantee-pair object #:optional caller)
  (unless (pair? object)
    (error "Not a pair" object caller)))

(define (except-last-pair list)
  (guarantee-pair list 'EXCEPT-LAST-PAIR)
  (if (not (pair? (cdr list)))
      '()
      (let ((head (cons (car list) '())))
        (let loop ((list (cdr list)) (previous head))
          (if (pair? (cdr list))
              (let ((new (cons (car list) '())))
                (set-cdr! previous new)
                (loop (cdr list) new))
              head)))))

)
