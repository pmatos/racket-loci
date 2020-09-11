#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require (for-syntax racket/base
                     racket/syntax
                     syntax/free-vars
                     syntax/parse)
         racket/contract
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/unix-socket
         "loci-log.rkt"
         (prefix-in ch: "locus-channel.rkt")
         "locus-transferable_gen.rkt"
         "locus_gen.rkt"
         "path.rkt"
         "utils.rkt")

(provide
 locus
 locus/context

 locus-pid
 locus-running?
 locus-exit-code
 locus-kill

 locus-enabled?
 
 (contract-out
  [dynamic-locus ((or/c module-path? path?) symbol? . -> . locus?)]
  [struct locus-dead-evt ((locus locus?))]
  [locus-wait (locus? . -> . exact-nonnegative-integer?)]
  [locus? (any/c . -> . boolean?)]
  [locus-channel-put/get ((or/c ch:locus-channel? locus?) any/c . -> . any/c)]
  [rename ch:locus-message-allowed? locus-message-allowed? (any/c . -> . boolean?)]
  [rename ch:locus-channel? locus-channel? (any/c . -> . boolean?)]
  [locus-channel-put ((or/c ch:locus-channel? locus?) ch:locus-message-allowed? . -> . void?)]
  [locus-channel-get ((or/c ch:locus-channel? locus?) . -> . any/c)]))

;; ---------------------------------------------------------------------------------------------------
;; For compatibility with places
(define (locus-enabled?) #true)

;; Extend locus channels
(define (locus-channel-put/get ch datum)
  (locus-channel-put ch datum)
  (locus-channel-get ch))

(define (resolve->channel o)
  (match o
    [(? locus? l) (local-locus-ch l)]
    [(? ch:locus-channel? l) l]))

(define (locus-channel-put ch datum)
  (log-debug "writing datum ~e to channel" datum)
  (ch:locus-channel-put (resolve->channel ch) datum))
(define (locus-channel-get ch)
  (log-debug "starting a read")
  (define d (ch:locus-channel-get (resolve->channel ch)))
  (log-debug "read datum ~e from channel" d)
  d)

;; ---------------------------------------------------------------------------------------------------
;; This file implement locus which run on the same machine as the master locus
(struct local-locus (ch subproc out-pump err-pump)
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
  #:property prop:output-port (struct-field-index ch)
  #:property prop:evt
  (lambda (s)
    (wrap-evt (ch:locus-channel-in (local-locus-ch s))
              (lambda (unix-ch)
                (ch:locus-channel-get (local-locus-ch s))))))

(struct locus-dead-evt (locus)
  #:property prop:evt
  (lambda (s)
    (wrap-evt (local-locus-subproc (locus-dead-evt-locus s))
              (lambda (subproc) s))))

;; dynamic-locus
;; Based on the implementation of place-process in
;; https://github.com/racket/racket/blob/master/pkgs/racket-benchmarks/tests/racket/
;;                                            /benchmarks/places/place-processes.rkt
(define (dynamic-locus mod func-name)
  (define (current-executable-path)
    (parameterize ([current-directory (find-system-path 'orig-dir)])
      (find-executable-path (find-system-path 'exec-file) #false)))
  (define (current-collects-path)
    (define p (find-system-path 'collects-dir))
    (if (complete-path? p)
        p
        (path->complete-path p (or (path-only (resolve-path (current-executable-path)))
                                   (find-system-path 'orig-dir)))))
  (define worker-cmdline-list (list (current-executable-path)
                                    "-X"
                                    (path->string (current-collects-path))
                                    "-e"
                                    "(eval (read))"))
  (log-debug "starting racket subprocess: ~e" worker-cmdline-list)
  (match-define-values (process-handle out in err)
    (apply subprocess
           #false
           #false
           #false
           worker-cmdline-list))
  (define stdout-pump
    (thread
     (thunk
      (log-debug "pump for stdout starting")
      (copy-port out (current-output-port) #:flush? #true)
      (log-debug "pump for stdout dying"))))
  (define stderr-pump
    (thread
     (thunk
      (log-debug "pump for stderr starting")
      (copy-port err (current-error-port) #:flush? #true)
      (log-debug "pump for stderr dying"))))

  (define tmp (make-temporary-file "loci~a"))
  (delete-file tmp)

  (log-debug "creating listener")
  (define listener (unix-socket-listen tmp))

  (define start-thread
    (thread
     (thunk
      (log-debug "sending debug message to locus")
      (define msg `(begin
                     (require loci/private/locus-channel
                              loci/private/path
                              racket/unix-socket)
                     (file-stream-buffer-mode (current-output-port) 'none)
                     (define-values (from-sock to-sock)
                       (unix-socket-connect ,(path->string tmp)))
                     ((dynamic-require (bytes->mod (quote ,(mod->bytes mod)))
                                       (quote ,func-name))
                      (locus-channel from-sock to-sock))))
      (log-debug "sending message into racket input port: ~e" msg)
      (write msg in)
      (flush-output in)
      (close-output-port in))))

  (log-debug "waiting for locus to accept connection")
  (define-values (from-sock to-sock)
    (unix-socket-accept listener))
  (thread-wait start-thread)

  (log-debug "successfully created locus")
  (local-locus (ch:locus-channel from-sock to-sock) process-handle stdout-pump stderr-pump))

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
