#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require "locus_gen.rkt"
         "locus-transferable_gen.rkt"
         (prefix-in ch: "locus-channel.rkt")
         racket/list
         racket/match
         racket/path

         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/free-vars))


(provide
 locus
 locus/context
 dynamic-locus

 locus-pid
 locus-wait
 locus-running?
 locus-exit-code
 locus-kill

 locus-dead-evt

 locus-channel-put/get
 ch:locus-message-allowed?
 locus-channel-put
 locus-channel-get)

;; ---------------------------------------------------------------------------------------------------
;; Extend locus channels

(define (locus-channel-put/get ch datum)
  (locus-channel-put ch datum)
  (locus-channel-get ch))

(define (resolve->channel o)
  (match o
    [(? locus? l) (local-locus-ch l)]
    [(? ch:locus-channel? l) l]))

(define (locus-channel-put ch datum)
  (printf "~e~n" `(locus-channel-put ,ch ,datum))
  (ch:locus-channel-put (resolve->channel ch) datum))
(define (locus-channel-get ch)
  (printf "~e~n" `(locus-channel-get ,ch))
  (ch:locus-channel-get (resolve->channel ch)))

;; ---------------------------------------------------------------------------------------------------
;; This file implement locus which run on the same machine as the master locus
(struct local-locus (ch subproc err)
  #:methods gen:locus
  [(define (locus-pid ll)
     (subprocess-pid (local-locus-subproc ll)))
   (define (locus-wait ll)
     (subprocess-wait (local-locus-subproc ll))
     (subprocess-status (local-locus-subproc ll)))
   (define (locus-running? ll)
     (eq? (subprocess-status (local-locus-subproc ll)) 'running))
   (define (locus-exit-code ll)
     (cond
       [(locus-running? ll) #false]
       [else (subprocess-status (local-locus-subproc ll))]))
   (define (locus-kill ll)
     (subprocess-kill (local-locus-subproc ll) #true))]
  #:property prop:input-port (struct-field-index ch)
  #:property prop:output-port (struct-field-index ch))

(struct locus-dead-evt (sp)
  #:property prop:evt (struct-field-index sp))

;; dynamic-locus
;; Based on the implementation of place-process in
;; https://github.com/racket/racket/blob/master/pkgs/racket-benchmarks/tests/racket/
;;                                            /benchmarks/places/place-processes.rkt
(define (dynamic-locus module-name func-name)
  (printf "dynamic-locus: ~a, ~a~n" module-name func-name)
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (module-name->bytes name)
    (cond
      [(path? name) (path->bytes name)]
      [(string? name) (string->bytes/locale name)]
      [(bytes? name) name]
      [(and (list? name)
            (>= (length name) 3)
            (eq? (first name) 'submod))
       (append `(submod ,(module-name->bytes (second name)))
               (drop name 2))]
      [else (error 'module->path "expects a path, string or submod declaration, got: ~a" name)]))
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

  (match-define-values (process-handle out in err)
    (apply subprocess
           #false
           #false
           #false
           worker-cmdline-list))
  (thread (thunk (copy-port out (current-output-port))))
  (thread (thunk (copy-port err (current-error-port))))
  (close-output-port in)

  (define tmp (make-temporary-file))
  (delete-file tmp)

  (define listener (unix-socket-listen tmp))

  (define msg `(begin
                 (require loci/private/locus-channel)
                 ((dynamic-require ,(let ([bstr (module-name->bytes module-name)])
                                      (if (bytes? bstr)
                                          `(bytes->path ,bstr)
                                          `(append (list ',(car bstr) (bytes->path ,(cadr bstr)))
                                                   ',(drop bstr 2))))
                                   (quote ,func-name))
                  (make-locus-channel/child))))
  (printf "msg: ~e~n" msg)
  (send/msg msg in)
  (printf "returning local-locus~n")
  (local-locus (ch:locus-channel out in) process-handle err))

(define-for-syntax locus-body-counter 0)

(define-syntax (locus stx)
   (syntax-case stx ()
    [(who ch body1 body ...)
     (if (eq? (syntax-local-context) 'module-begin)
         ;; when a `place' form is the only thing in a module body:
         #`(begin #,stx)
         ;; normal case:
         (let ()
           (unless (syntax-transforming-module-expression?)
             (raise-syntax-error #false "can only be used in a module" stx))
           (unless (identifier? #'ch)
             (raise-syntax-error #false "expected an identifier" stx #'ch))
           (set! locus-body-counter (add1 locus-body-counter))
           (define module-name-stx
             (datum->syntax stx
               (string->symbol
                (format "locus-body-~a" locus-body-counter))))
           (with-syntax ([internal-def-name
                          (syntax-local-lift-module
                           #`(module* #,module-name-stx #false
                               (provide main)
                               (define (main ch)
                                 body1 body ...)
                               ;; The existence of this submodule makes the
                               ;; enclosing submodule preserved by `raco exe`:
                               (module declare-preserve-for-embedding '#%kernel)))])
             #`(locus/proc (#%variable-reference) '#,module-name-stx 'who))))]
     [(_ ch)
      (raise-syntax-error #false "expected at least one body expression" stx)]))

(define (locus/proc vr submod-name who)
  (printf "vr: ~a~n" vr)
  (printf "submod-name: ~a~n" submod-name)
  (printf "who: ~a~n" who)
  (define name
    (resolved-module-path-name
     (variable-reference->resolved-module-path
      vr)))
  (when (and (symbol? name)
             (not (module-predefined? `(quote ,name))))
    (error who "the enclosing module's resolved name is not a path or predefined"))
  (define submod-ref
    (match name
      [(? symbol?) `(submod (quote ,name) ,submod-name)]
      [(? path?) `(submod ,name ,submod-name)]
      [`(,p ,s ...) `(submod ,(if (symbol? p) `(quote ,p) p) ,@s ,submod-name)]))
  (dynamic-locus submod-ref 'main))

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
           (unless (ch:locus-message-allowed? e)
             (raise-arguments-error 'locus/context
                                    "free variable values must be allowable as locus messages"
                                    (symbol->string (syntax-e n)) e)))
         (locus-channel-put l vec)
         l)]))

(module+ test

  (require rackunit)

  (test-case "Syntax locus/context tests"
    (define v 2)
    (check-= v (locus-channel-get
                (locus/context ch
                  (locus-channel-put ch v)))))

  (test-case "Syntax locus tests"
    (check-= 2 (locus-channel-get
                (locus ch
                  (locus-channel-put ch 2))))))
