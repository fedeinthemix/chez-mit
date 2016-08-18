#!chezscheme
(library (mit hash-tables)
  (export make-key-weak-eqv-hash-table
          make-key-weak-eq-hash-table
          make-weak-eq-hash-table
          make-eq-hash-table
          hash-table/get
          hash-table/put!
          eqv-hash-mod
          equal-hash-mod
          weak-hash-table/constructor
          strong-hash-table/constructor
          hash-table/intern!
          hash-table/key-list
          hash
          hash-table->alist)
  (import (except (chezscheme) error assert sort)
          (mit core)
          (mit curry)
          )

(define make-key-weak-eqv-hash-table make-weak-eqv-hashtable)

(define make-key-weak-eq-hash-table make-weak-eq-hashtable)

(define make-weak-eq-hash-table make-key-weak-eq-hash-table)

(define make-eq-hash-table make-key-weak-eq-hash-table)
;;(define make-eq-hash-table make-eq-hashtable)

(define hash-table/get hashtable-ref)

(define hash-table/put! hashtable-set!)

(define (hash-table/intern! table key get-default)
  (let ((default (get-default)))
    (unless (hashtable-contains? table key)
      (hashtable-set! table key default))
    (hashtable-ref table key default)))

;; from schez-scheme s/newhash.ss
(define eqv-generic?
  (lambda (x)
    ;; all numbers except fixnums must go through generic hashtable
    (and (number? x)
         (or (flonum? x) (bignum? x) (ratnum? x) (exact? x) (inexact? x)))))

(define (eqv-hash key)
  (equal-hash key)
  ;; (if (eqv-generic? key)
  ;;     ;; equal-hash passes numbers to number-hash, as the internal eqv-hash
  ;;     (equal-hash key)
  ;;     (symbol-hash key))
  )

;; from MIT-Scheme
;; (define (eqv-hash-mod key modulus)
;;   (remainder (eqv-hash key) modulus))
(define* (eqv-hash-mod key #:optional modulus) (eqv-hash key))

;; (define (equal-hash-mod key modulus)
;;   (remainder (equal-hash key) modulus))
(define* (equal-hash-mod key #:optional modulus) (equal-hash key))

(define* (hash key #:optional modulus)
  (if (default-object? modulus)
      (equal-hash key)
      (equal-hash-mod key modulus)))

;; XXX: since we do not have a generic weak hashtable constructor,
;; we make weak-pairs in generic/weak.scm normal pairs.
(define weak-hash-table/constructor
  (lambda* (key-hash key=? #:optional rehash-after-gc?)
    (lambda () (make-hashtable key-hash key=?))))

(define strong-hash-table/constructor
  (lambda* (key-hash key=? #:optional rehash-after-gc?)
    (lambda () (make-hashtable key-hash key=?))))

(define hash-table/key-list hashtable-keys)

(define (hash-table->alist ht)
  (let-values (((keys vals) (hashtable-entries ht)))
    (vector->list
     (vector-map (lambda (k v) (cons k v)) keys vals))))

)
