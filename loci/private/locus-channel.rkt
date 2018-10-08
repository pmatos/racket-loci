#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/fasl
         racket/serialize)

(provide
 make-locus-channel/child
 locus-channel
 locus-channel?
 locus-channel-put
 locus-channel-get)

;; ---------------------------------------------------------------------------------------------------

(struct locus-channel (in out))

(define (make-locus-channel/child)
  (locus-channel (current-input-port) (current-output-port)))

(define (locus-channel-put ch datum)
  (define out (locus-channel-out ch))
  (write (s-exp->fasl (serialize datum)) out)
  (flush-output out))

(define (locus-channel-get ch datum)
  (deserialize (fasl->s-exp (read (locus-channel-in ch)))))
