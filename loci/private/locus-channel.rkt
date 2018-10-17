#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         racket/fasl
         racket/serialize)

(provide
 (contract-out
  [struct locus-channel ((in input-port?)
                         (out output-port?))]
  [locus-channel-put (locus-channel? locus-message-allowed? . -> . void?)]
  [locus-channel-get (locus-channel? . -> . locus-message-allowed?)]
  [locus-message-allowed? (any/c . -> . boolean?)]))

;; ---------------------------------------------------------------------------------------------------

;; A locus channel consists of two file-streams to communicate with the locus
;; in is connected to the locus stdout
;; out is connected to the locus stdin
;; therefore one reads from in and writes to out
(struct locus-channel (in out)
  #:property prop:evt (lambda (s)
                        (wrap-evt (locus-channel-in s)
                                  (lambda (ch) s))))

(define (locus-channel-put ch datum)
  (define out (locus-channel-out ch))
  (s-exp->fasl datum out)
  (flush-output out))

(define (locus-channel-get ch)
  (fasl->s-exp (locus-channel-in ch)))

(define (locus-message-allowed? v)
  (or (char? v)
      (void? v)
      (number? v)
      (eof-object? v)
      (symbol? v)
      (keyword? v)
      (string? v)
      (bytes? v)
      (path? v)
      (null? v)
      (boolean? v)
      (and (pair? v)
           (locus-message-allowed? (car v))
           (locus-message-allowed? (cdr v)))
      (and (vector? v)
           (for/and ([i (in-vector v)])
             (locus-message-allowed? i)))
      (and (hash? v)
           (for/and ([(k i) (in-hash v)])
             (and (locus-message-allowed? k)
                  (locus-message-allowed? i))))
      (and (box? v)
           (locus-message-allowed? (unbox v)))
      (and (prefab-struct-key v) #true)))
