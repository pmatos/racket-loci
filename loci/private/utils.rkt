#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract)

(provide
 (contract-out
  [copy-port (->* (input-port? output-port?) #:rest (listof output-port?) void?)]))

;; ---------------------------------------------------------------------------------------------------

(define (copy-port src dest . dests)
  (unless (input-port? src)
    (raise-type-error 'copy-port "input-port" src))
  (for-each
   (lambda (dest)
     (unless (output-port? dest)
       (raise-type-error 'copy-port "output-port" dest)))
   (cons dest dests))
  (let ([s (make-bytes 4096)]
        [dests (cons dest dests)])
    (let loop ()
      (let ([c (read-bytes-avail! s src)])
        (cond
          [(number? c)
           (let loop ([dests dests])
             (unless (null? dests)
               (let loop ([start 0])
                 (unless (= start c)
                   (let ([c2 (write-bytes-avail s (car dests) start c)])
                     (loop (+ start c2)))))
               (loop (cdr dests))))
           (loop)]
          [(procedure? c)
           (let ([v (let-values ([(l col p) (port-next-location src)])
                      (c (object-name src) l col p))])
             (let loop ([dests dests])
               (unless (null? dests)
                 (write-special v (car dests))
                 (loop (cdr dests)))))
           (loop)]
          [else
           ;; Must be EOF
           (void)])))))
