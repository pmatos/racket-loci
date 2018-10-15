#lang racket
;; ---------------------------------------------------------------------------------------------------

(require racket/serialize)

;; ---------------------------------------------------------------------------------------------------

(serializable-struct msg-id (worker-id))
(serializable-struct compute-v (sender v))
(serializable-struct ask-factorial (sender v))
(serializable-struct answer-factorial (sender v fact-v))

(define (igensym)
  (string->symbol
   (symbol->string
    (gensym))))

;; Test of loci calculating the factorial of every number between 0 - 100
;; except that they won't calculate a factorial if it's already done, so they ask the
;; master first to see if the master knows the answer first.
(define (worker-factorial-go ch)
  (printf "worker-factorial-go~n")

  ;; Get id
  (define id
    (match (place-channel-get ch)
      [(struct msg-id (id)) id]
      [m (error 'worker-factorial-go "unexpected message, expected msg-id, got: ~a" m)]))
  (printf "worker ~a here~n" id)

  ;; Get value
  (define v
    (match (place-channel-get ch)
      [(struct compute-v (_ v)) v]
      [m (error 'worker-factorial-go "unexpected message, expected compute-v, got: ~a" m)]))

  (define factorial
    (compute-factorial id ch v))

  (place-channel-put ch (answer-factorial id v factorial)))

(define (compute-factorial id ch v)
  (printf "compute-factorial ~a ~a~n" id v)

  ;; Ask master for the factorial of v
  (define v-fact (place-channel-put/get ch (ask-factorial id v)))

  (match v-fact
    [(struct answer-factorial (_ _ #false)) (* v (compute-factorial (- v 1)))]
    [(struct answer-factorial (_ _ f)) f]
    [m (error 'compute-factorial "unexpected message, expected answer-factorial, got: ~a" m)]))

(define (print-factorial-table table)
  (for ([i (range 1 (add1 (hash-count table)))])
    (printf "~a ~a~n" (add1 i) (vector-ref table i))))

(define (main cores N)
  (define cache (make-hasheq))

  (define workers
    (for/hasheq ([i (range cores)])
      (values (igensym) (place ch (worker-factorial-go ch)))))

  (define work (shuffle (range 1 (add1 N))))
  (define-values (work-now work-later)
    (split-at work cores))

  ;; Send initial message to all
  (for ([(id w) (in-hash workers)]
        [v (in-list work-now)])
    (place-channel-put w (msg-id id))
    (place-channel-put w (compute-v 'master v)))

  (let loop ([active-workers workers]
             [remaining-work work-later])

    (printf "Remaining workers: ~a~n" active-workers)
    (printf "Remaining work: ~a~n" remaining-work)
    (printf "cache: ~a~n" cache)

    (cond
      [(and (null? remaining-work)
            (null? active-workers))
       (printf "All done~n")
       (print-factorial-table cache)]
      [else

       (sync
        (handle-evt
         (apply choice-evt (map place-dead-evt (hash-values workers)))
         (lambda (l)
           (printf "A place finished~n")
           (unless (null? remaining-work)
             (define new-place (place ch (worker-factorial-go ch)))
             (define id (igensym))
             (place-channel-put new-place (msg-id id))
             (place-channel-put new-place (compute-v 'master (car remaining-work)))
             (printf "Requesting new place ~a for ~a~n" id (car remaining-work))
             (loop (hash-set
                    (for/hasheq ([(id l) (in-hash active-workers)]
                                 #:when (place-running? l))
                      (printf "worker ~a still alive~n" id)
                      (values id l))
                    id new-place)
                   (rest remaining-work)))))

        (handle-evt
         (apply choice-evt (hash-values workers))
         (match-lambda
           [(struct ask-factorial (w v))
            (printf "Worker asking for ~a!~n" v)
            (cond
              [(hash-ref cache v #false)
               => (lambda (f)
                   (place-channel-put (hash-ref active-workers w)
                                      (answer-factorial 'master v f)))]
              [else
               (printf "workers: ~a~n" active-workers)
               (place-channel-put (hash-ref active-workers w)
                                  (answer-factorial 'master v #false))])
            (loop active-workers remaining-work)]

           [(struct answer-factorial (w v f))
            (printf "Worker giving factorial answer ~a! = ~a~n" v f)
            (hash-set! cache v f)

            (place-kill (hash-ref active-workers w))

            (define new-place (place ch (worker-factorial-go ch)))
            (define id (igensym))
            (place-channel-put new-place (msg-id id))
            (place-channel-put new-place (compute-v 'master (car remaining-work)))
            (printf "Requesting new place ~a for ~a~n" id (car remaining-work))
            (loop (hash-set
                   (for/hasheq ([(id l) (in-hash active-workers)]
                                #:when (place-running? l))
                     (printf "worker ~a still alive~n" id)
                     (values id l))
                   id new-place)
                  (rest remaining-work))]

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
