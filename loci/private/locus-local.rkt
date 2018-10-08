#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require "locus_gen.rkt"
         "locus-transferable_gen.rkt"
         "locus-channel.rkt"
         racket/match
         racket/path

         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/free-vars
                     syntax/strip-context))


(provide
 locus
 locus/context
 dynamic-locus

 locus-pid
 locus-wait
 locus-running?
 locus-exit-code
 locus-kill

 locus-channel-put/get
 (rename-out
  [locus-channel-put+ locus-channel-put]
  [locus-channel-get+ locus-channel-get]))

;; ---------------------------------------------------------------------------------------------------
;; Extend locus channels

(define (locus-channel-put/get ch datum)
  (locus-channel-put+ ch datum)
  (locus-channel-get+ ch))

(define (resolve->channel o)
  (match o
    [(? locus? l) (local-locus-ch l)]
    [(? locus-channel? l) l]))

(define (locus-channel-put+ ch datum)
  (locus-channel-put (resolve->channel ch) datum))
(define (locus-channel-get+ ch datum)
  (locus-channel-get (resolve->channel ch)))

;; ---------------------------------------------------------------------------------------------------
;; This file implement locus which run on the same machine as the master locus
(struct local-locus (ch subproc err)
  #:methods gen:locus
  [(define (locus-pid ll)
     (subprocess-pid (local-locus-subproc ll)))
   (define (locus-wait ll)
     (subprocess-wait (local-locus-subproc ll))
     (subprocess-status ll))
   (define (locus-running? ll)
     (eq? (subprocess-status (local-locus-subproc ll)) 'running))
   (define (locus-exit-code ll)
     (cond
       [(locus-running? ll) #false]
       [else (subprocess-status (local-locus-subproc ll))]))
   (define (locus-kill ll)
     (subprocess-kill (local-locus-subproc ll) #true))])

;; dynamic-locus
;; Based on the implementation of place-process in
;; https://github.com/racket/racket/blob/master/pkgs/racket-benchmarks/tests/racket/
;;                                            /benchmarks/places/place-processes.rkt
(define (dynamic-locus module-name func-name)
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (module-name->bytes name)
    (cond
      [(path? name) (path->bytes name)]
      [(string? name) (string->bytes/locale name)]
      [(bytes? name) name]
      [(and (list? name)
            (= 3 (length name))
            (eq? (car name) 'submod))
       `(submod ,(module-name->bytes (cadr name)) ,(caddr name))]
      [else (error 'module->path "expects a path, string or submod declaration")]))
  (define (current-executable-path)
    (parameterize ([current-directory (find-system-path 'orig-dir)])
      (find-executable-path (find-system-path 'exec-file) #false)))
  (define (current-collects-path)
    (define p (find-system-path 'collects-dir))
    (if (complete-path? p)
        p
        (path->complete-path p (or (path-only (current-executable-path))
                                  (find-system-path 'orig-dir)))))
  (define worker-cmdline-list (list (current-executable-path)
                                    "-X"
                                    (path->string (current-collects-path))
                                    "-e"
                                    "(eval (read))"))

  (let-values ([(process-handle out in err)
                (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
    (send/msg `((dynamic-require ,(let ([bstr (module-name->bytes module-name)])
                                    (if (bytes? bstr)
                                        `(bytes->path ,bstr)
                                        `(list ',(car bstr) (bytes->path ,(cadr bstr)) ',(caddr bstr))))
                                 (quote ,func-name)))
            in)

    (local-locus (locus-channel out in) process-handle err)))

(define-syntax (locus stx)
  (syntax-case (replace-context #'here stx) ()
    [(_ module-name (name ch) body ...)
     #'(module module-name racket/base
         (require "locus-local.rkt")
         (provide name)
         (define (name)
           (let ([ch (make-locus-channel/child)])
             body ...)))]))

(define-syntax (locus/context stx)
  (syntax-parse stx
    [(_ ch:id body:expr ...)
     (define b #'(lambda (ch) body ...))
     (define/with-syntax b* (local-expand b 'expression null))
     (define/with-syntax (fvs ...) (free-vars #'b*))
     (define/with-syntax (i ...)
       (for/list ([(v i) (in-indexed (syntax->list #'(fvs ...)))]) i))
     (define/with-syntax (v l) (generate-temporaries '(v l)))
     #'(let ()
         (define l
           (locus ch
             (let* ([v (locus-channel-get ch)]
                    [fvs (vector-ref v i)] ...)
               (b* ch))))
         (define vec (vector fvs ...))
         (for ([e (in-vector vec)]
               [n (in-list (syntax->list (quote-syntax (fvs ...))))])
           (unless (locus-message-allowed? e)
             (raise-arguments-error 'locus/context
                                    "free variable values must be allowable as locus messages"
                                    (symbol->string (syntax-e n)) e)))
         (locus-channel-put l vec)
         l)]))
