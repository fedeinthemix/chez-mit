(import (mit apply-hook)
        (srfi :64))

(test-begin "apply-hook-test")

(define fbe-ah (make-apply-hook (lambda (x) (cons 'ah-proc x)) 'ah-extra))

(test-equal "apply" '(ah-proc . x) (fbe-ah 'x))

(test-equal "apply-get-proc" '(ah-proc . get-proc) (fbe-ah 'get-proc))

(test-equal "arity" '(1 . 1) (procedure-arity fbe-ah))

(test-equal "hook?-false" #f (apply-hook? (lambda x x)))

(test-equal "hook?-true" #t (apply-hook? fbe-ah))

(test-equal "extra" 'ah-extra (apply-hook-extra fbe-ah))

(test-equal "proc" '(ah-proc . x) ((apply-hook-procedure fbe-ah) 'x))

(test-end "apply-hook-test")
