#lang racket
;; ---------------------------------------------------------------------------------------------------

(require loci
         racket/serialize)

;; ---------------------------------------------------------------------------------------------------

(serializable-struct msg-id (worker-id))
(serializable-struct compute-v (sender v))
(serializable-struct ask-factorial (sender v))
(serializable-struct answer-factorial (sender v fact-v))

;; Test of loci calculating the factorial of every number between 0 - 100
;; except that they won't calculate a factorial if it's already done, so they ask the
;; master first to see if the master knows the answer first.
(define (worker-factorial-go ch)
  ;; Get id
  (define id
    (match (locus-channel-get ch)
      [(struct msg-id (id)) id]
      [m (error 'worker-factorial-go "unexpected message, expected msg-id, got: ~a" m)]))


  ;; Get value
  (define v
    (match (locus-channel-get ch)
      [(struct compute-v (_ v)) v]
      [m (error 'worker-factorial-go "unexpected message, expected compute-v, got: ~a" m)]))

  (define factorial
    (compute-factorial id ch v))

  (locus-channel-put ch (answer-factorial id v factorial)))

(define (compute-factorial id ch v)
  ;; Ask master for the factorial of v
  (define v-fact (locus-channel-put/get ch (ask-factorial id v)))

  (match v-fact
    [(struct answer-factorial (_ _ #false)) (* v (compute-factorial (- v 1)))]
    [(struct answer-factorial (_ _ f)) f]
    [m (error 'compute-factorial "unexpected message, expected answer-factorial, got: ~a" m)]))

(define (print-factorial-table table)
  (for ([i (range 1 (add1 (vector-length table)))])
    (printf "~a ~a~n" (add1 i) (vector-ref table i))))

(define (main cores N)
  (define cache (make-hasheq))

  (define workers
    (for/hasheq ([i (range cores)])
      (values (gensym) (locus ch (worker-factorial-go ch)))))

  (define work (shuffle (range 1 (add1 N))))
  (define-values (work-now work-later)
    (split-at work cores))

  ;; Send initial message to all
  (for ([(id w) (in-hash workers)]
        [v (in-list work-now)])
    (locus-channel-put w (msg-id id))
    (locus-channel-put w (compute-v 'master v)))

  (let loop ([active-workers workers]
             [remaining-work work-later])

    (printf "Remaining workers: ~a~n" (hash-count workers))
    (printf "Remaining work: ~a~n" remaining-work)

    (cond
      [(and (null? remaining-work)
            (null? active-workers))
       (printf "All done~n")
       (print-factorial-table cache)]
      [else

       (sync
        (handle-evt
         (apply choice-evt (map locus-dead-evt (hash-values workers)))
         (lambda (l)
           (printf "A locus finished~n")
           (unless (null? remaining-work)
             (define new-locus (locus ch (worker-factorial-go ch)))
             (define id (gensym))
             (locus-channel-put new-locus (msg-id id))
             (locus-channel-put new-locus (compute-v 'master (car remaining-work)))

             (loop (hash-set
                    (for/hasheq ([(id l) (in-hash active-workers)]
                                 #:when (locus-running? l))
                      (values id l))
                    id new-locus)
                   (rest remaining-work)))))

        (handle-evt
         (apply choice-evt (hash-values workers))
         (match-lambda
           [(struct ask-factorial (w v))
            (cond
              [(vector-ref cache v)
               => (lambda (f) (locus-channel-put w (answer-factorial 'master v f)))]
              [else (locus-channel-put w (answer-factorial 'master v #false))])]

           [(struct answer-factorial (_ v f))
            (vector-set! cache v f)]

           [msg
            (error 'main "unexpected msg, got: ~a" msg)])))])))


(module+ main

  (require racket/os)

  (printf "Parent PID: ~a~n" (getpid))

  (define-values (cores N)
    (command-line
     #:args (cores factmax)
     (values
      (string->number cores)
      (string->number factmax))))


  (main cores N))
