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

;;; Comments

;; This file provides procedures used to extract the arity of
;; procedures from the R6RS Standard and Chez Scheme User Guide (LaTeX
;; files).

;;; Code

(import (irregex) (matchable)
        (only (srfi :1) delete-duplicates))

(define rx-proc-name 'proc)
(define rx-args-name 'args)

(define r6rs-proto-rx
  (irregex
   `(seq (or "\\proto{" "\\rproto{") (* white)
         (submatch-named ,rx-proc-name (+ (~ #\}))) ; proc name
         (* white) "}" (* white) "{" (* white)
         (submatch-named ,rx-args-name (* any))     ; proc args
         (* white) "}" (* white) "{" (* white) "procedure" (* white) "}")))

(define r6rs-dots-rx
  (irregex '(or "\\dotsfoo"
                "$\\ldots$"
                (seq "\\dotsfoo{" (* space) "}"))))

(define csug-formdef-rx
  (irregex
   `(seq "\\formdef{" (* white)
         (submatch-named ,rx-proc-name (+ (~ #\}))) ; proc name
         (* white) "}" (* white)
         "{" (* white) "\\categoryprocedure" (* white) "}"
         (* white) "{" (* white) "(" (+ (~ white)) (+ white)
         (submatch-named ,rx-args-name (* any))     ; proc args
         (* white) ")" (* white) "}")))

(define csug-dots-rx
  (irregex '(: "\\dots")))

(define procdef-rx (make-parameter r6rs-proto-rx))
(define dots-rx (make-parameter r6rs-dots-rx))

(define (get-proc-name m)
  (string->symbol (irregex-match-substring m rx-proc-name)))

(define (args-has-dots? str)
  (irregex-search (dots-rx) str))

(define (args-split-at-dots str)
  (irregex-split (dots-rx) str))

(define (args-split str)
  (irregex-split '(: (+ space)) str))

(define (args-count ls)
  (length ls))

(define (proc-args->arity str)
  (if (args-has-dots? str)
      (let ((ls-req/rest (args-split-at-dots str)))
        (cons (- (args-count (args-split (car ls-req/rest))) 1) #f))
      (let* ((ls-req (args-split str))
             (req-no (args-count ls-req)))
        (cons req-no req-no))))

(define (alist-keys alist)
  (map car alist))

(define (alist-values alist)
  (map cdr alist))

(define (%arities-union a1 a2)
  (match (cons a1 a2)
    (((a1-min . a1-max) . (a2-min . a2-max))
     (let ((a-max (if (or (not a1-max) (not a2-max)) #f (max a1-max a2-max))))
       (cons (min a1-min a2-min) a-max)))))

;; XXX: this is of course an approximation, but backed by common-sense.
(define (union-arities key alist)
  (let ((arities (filter (lambda (e) (eq? key (car e))) alist)))
    (if (> (length arities) 1)
        (cons key
              (fold-left %arities-union `(,(greatest-fixnum) . 0)
                         (alist-values arities)))
        (car arities))))

(define (union-arities-all alist)
  (map (lambda (k) (union-arities k alist))
       (delete-duplicates (alist-keys alist))))

(define (get-proc-args m)
  (irregex-match-substring m rx-args-name))

(define (make-proc-entry m)
  (cons (get-proc-name m) (proc-args->arity (get-proc-args m))))

;; XXX: This relies on the definition not being split on multiple
;; lines.
(define (extract-arities-from-file fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ((acc '())
                 (line (get-line (current-input-port))))
        (if (eof-object? line)
            (union-arities-all acc)
            (let ((m (irregex-search (procdef-rx) line)))
              (loop (if (irregex-match-data? m)
                        (cons (make-proc-entry m) acc)
                        acc)
                    (get-line (current-input-port)))))))))

;;; Files

(define r6rs-doc-dir "/home/beffa/Downloads/r6rs/document")

(define r6rs-doc-files
  (list "base.tex"
        "unicode.tex"
        "bytevector.tex"
        "list.tex"
        "sort.tex"
        "control.tex"
        "records.tex"
        "exc.tex"
        "io.tex"
        "files.tex"
        "programlib.tex"
        "arith.tex"
        "syntax-case.tex"
        "hashtable.tex"
        "enum.tex"
        "complib.tex"
        "eval.tex"
        "setcar.tex"
        "stringset.tex"
        "r5rscompat.tex"))

(define csug-doc-dir "/home/beffa/src/chez-git/ChezScheme/csug")

(define csug-doc-files
  (list "debug.stex"
        "foreign.stex"
        "binding.stex"
        "control.stex"
        "objects.stex"
        "numeric.stex"
        "io.stex"
        "libraries.stex"
        "syntax.stex"
        "system.stex"
        "smgmt.stex"
        "expeditor.stex"
        "threads.stex"))

(define (file-absolute-path dir fn)
  (string-append dir "/" fn))

;;; Main function

(define (r6rs-doc-arities-all)
  (parameterize ((procdef-rx r6rs-proto-rx)
                 (dots-rx r6rs-dots-rx))
    (let ((files-ls
           (map (lambda (f) (file-absolute-path r6rs-doc-dir f))
                r6rs-doc-files)))
      (apply append (map extract-arities-from-file files-ls)))))

(define (csug-doc-arities-all)
  (parameterize ((procdef-rx csug-formdef-rx)
                 (dots-rx csug-dots-rx))
    (let ((files-ls
           (map (lambda (f) (file-absolute-path csug-doc-dir f))
                csug-doc-files)))
      (apply append (map extract-arities-from-file files-ls)))))
