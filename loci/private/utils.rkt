#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract)

(provide
 (contract-out
  [copy-port (->* (input-port? output-port?) #:rest (listof output-port?) void?)]))

;; ---------------------------------------------------------------------------------------------------

(define (copy-port src dest . dests*)
  (unless (input-port? src)
    (raise-type-error 'copy-port "input-port" src))
  (for-each
   (lambda (dest)
     (unless (output-port? dest)
       (raise-type-error 'copy-port "output-port" dest)))
   (cons dest dests*))


  (define sz 4096)
  (define s (make-bytes sz))
  (define dests (cons dest dests*))

  (let loop ()
    (define c (read-bytes-avail! s src))
    (cond
      [(number? c)
       (for ([dest (in-list dests)])
         (for/fold ([start 0]
                    #:result (void))
                   (#:unless (= start c)
                    [c2 (write-bytes-avail s dest start c)])
           (+ start c2)))
       (loop)]

    [(procedure? c)
     (let ([v (let-values ([(l col p) (port-next-location src)])
                (c (object-name src) l col p))])
       (for ([dest (in-list dests)])
         (write-special v dest)))
     (loop)]

    [else
     ;; Must be EOF
     (void)])))
