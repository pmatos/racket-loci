#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         racket/fasl
         racket/serialize)

(provide
 (contract-out
  [locus-channel (input-port? output-port? . -> . locus-channel?)]
  [locus-channel? (any/c . -> . boolean?)]
  [locus-channel-put (locus-channel? locus-message-allowed? . -> . void?)]
  [locus-channel-get (locus-channel? . -> . any/c)]
  [locus-message-allowed? (any/c . -> . boolean?)]))

;; ---------------------------------------------------------------------------------------------------

;; A locus channel consists of two file-streams to communicate with the locus
;; in is connected to the locus stdout
;; out is connected to the locus stdin
;; therefore one reads from in and writes to out
(struct locus-channel (in out)
  #:property prop:input-port (struct-field-index in)
  #:property prop:output-port (struct-field-index out))

(define (locus-channel-put ch datum)
  (define out (locus-channel-out ch))
  (write (s-exp->fasl (serialize datum)) out)
  (flush-output out))

(define (locus-channel-get ch)
  (deserialize (fasl->s-exp (read (locus-channel-in ch)))))

(define locus-message-allowed? serializable?)
