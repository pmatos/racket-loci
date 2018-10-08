#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/generic)


(provide gen:locus
         locus/c
         locus?
         locus-pid
         locus-wait
         locus-running?
         locus-exit-code
         locus-kill)

;; ---------------------------------------------------------------------------------------------------

;; A locus wraps a call to a subprocess, whose executable is the racket
;; executable that's running when the call is made
(define-generics locus
  (locus-pid locus)
  (locus-wait locus)
  (locus-running? locus)
  (locus-exit-code locus)
  (locus-kill locus))
