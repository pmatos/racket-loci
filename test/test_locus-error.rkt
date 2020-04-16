#lang racket
;; ---------------------------------------------------------------------------------------------------

(require loci)

;; ---------------------------------------------------------------------------------------------------

(define (go)
  (locus l
    (printf "Testing stdout")
    (fprintf (current-error-port) "Testing stderr")
    (error 'go "locus failed on purpose")))

(module+ main

  (printf "Starting core~n")
  (define r (locus-wait (go)))
  (printf "Locus finished with ~a~n" r)
  (exit r))
