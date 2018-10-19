#lang racket
;; ---------------------------------------------------------------------------------------------------

(require loci)

;; ---------------------------------------------------------------------------------------------------

(define (go)
  (printf "Hello World from Locus~n"))

(define (main)
  (define l
    (locus ch
      (go)))
  (locus-wait l))

(module+ main
  (printf "starting up~n")
  (main))
