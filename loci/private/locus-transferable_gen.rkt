#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/generic)

(provide gen:locus-transferable
         locus-transferable/c
         locus-transferable?
         byte-encode
         byte-decode)

;; ---------------------------------------------------------------------------------------------------

;; A locus transferable is any message that can be sent accross a locus channel
;; Any such message needs to implement the locus-transferable methods
(define-generics locus-transferable
  (byte-encode locus-transferable)
  (byte-decode locus-transferable))
