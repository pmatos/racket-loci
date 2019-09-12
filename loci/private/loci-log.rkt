#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require (for-syntax racket/base)
         racket/os)

(provide log-debug)

;; ---------------------------------------------------------------------------------------------------

;; Locus Logger
(define-logger loci)
(error-print-width 1024)

(define-syntax (log-debug stx)
  (syntax-case stx ()
    [(_ msg)
     #'(log-loci-debug (string-append "[~a]: "  msg) (getpid))]
    [(_ msg as ...)
     #'(log-loci-debug (string-append "[~a]: " msg) (getpid) as ...)]))
