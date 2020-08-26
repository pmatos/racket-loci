#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/match
         racket/path
         syntax/modresolve)

(provide mod->bytes bytes->mod)

;; ---------------------------------------------------------------------------------------------------

;; In order to send the module path through the channel we need to serialize and
;; deserialize paths. So we use bytes for serialization.
(define (mod->bytes mod-path)
  (match (resolve-module-path-index (module-path-index-join mod-path #false))
    [`(submod ,(? path? ps) ,ss ...)
     `(submod ,(path->bytes ps) ,@ss)]
    [`(submod ,(? symbol? ps) ,ss ...)
     `(submod ,ps ,@ss)]
    [(? path? p) (path->bytes p)]
    [(? symbol? s) s]))

;; We need this to be called on the remote side to massage the received value
(define (bytes->mod mod-path)
  (match mod-path
    [`(submod ,(? bytes? p) ,ss ...)
     `(submod ,(bytes->path p) ,@ss)]
    [`(submod ,(? symbol? s) ,ss ...)
     `(submod ,s ,@ss)]
    [(? bytes? p) (bytes->path p)]
    [(? symbol? s) s]))
