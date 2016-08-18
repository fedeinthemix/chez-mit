(import (mit arity)
        (srfi :64))

;; test procedures

(define fbe-p1 (lambda (x) x))

(define fbe-p2 (lambda x x))

(define fbe-p3
  (case-lambda
    ((x) x)
    ((x y) (cons x y))))

(define fbe-p4
  (case-lambda
    ((x) x)
    ((x . y) (cons x y))))

;; start tests

(test-begin "procedure-arity")

(test-equal '(1 . 1) (procedure-arity fbe-p1))

(test-equal '(0 . #f) (procedure-arity fbe-p2))

(test-equal '(1 . 2) (procedure-arity fbe-p3))

(test-equal '(1 . #f) (procedure-arity fbe-p4))

(test-end "procedure-arity")
