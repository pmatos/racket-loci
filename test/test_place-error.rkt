#lang racket
;; ---------------------------------------------------------------------------------------------------

(define (go)
  (place p
    (printf "Testing stdout")
    (fprintf (current-error-port) "Testing stderr")
    (error 'go "place failed on purpose")))

(module+ main

  (printf "Starting core~n")
  (define r (place-wait (go)))
  (printf "Place finished with ~a~n" r))
