#lang racket
;; ---------------------------------------------------------------------------------------------------

(require loci)

;; ---------------------------------------------------------------------------------------------------

(define (go n)
  (locus/context l
         (let ([v (vector 0.0)])
           (let loop ([i 3000000000])
             (unless (zero? i)
               (vector-set! v 0 (+ (vector-ref v 0) 1.0))
               (loop (sub1 i)))))
         (printf "Locus ~a done~n" n)
         n))

(module+ main

  (define cores
    (command-line
     #:args (cores)
     (string->number cores)))

  (time
   (map locus-wait
        (for/list ([i (in-range cores)])
          (printf "Starting core ~a~n" i)
          (go i)))))
