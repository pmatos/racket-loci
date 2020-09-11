#lang racket
;; ---------------------------------------------------------------------------------------------------

(require loci
         racket/serialize)

;; ---------------------------------------------------------------------------------------------------

(struct msg-id
  (worker-id)
  #:prefab)
(struct compute-v
  (sender v)
  #:prefab)
(struct ask-factorial
  (sender v)
  #:prefab)
(struct answer-factorial
  (sender v fact-v)
  #:prefab)
(struct worker-done
  (sender)
  #:prefab)

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
    (match (locus-channel-get ch)
      [(struct msg-id (id)) id]
      [m (error 'worker-factorial-go "unexpected message, expected msg-id, got: ~a" m)]))
  (printf "worker ~a here~n" id)

  ;; Get value
  (define v
    (match (locus-channel-get ch)
      [(struct compute-v (_ v)) v]
      [m (error 'worker-factorial-go "unexpected message, expected compute-v, got: ~a" m)]))

  (define factorial
    (compute-factorial id ch v))

  (locus-channel-put ch (answer-factorial id v factorial))
  (locus-channel-put ch (worker-done id)))

(define (compute-factorial id ch v)
  (printf "compute-factorial ~a ~a~n" id v)

  ;; Ask master for the factorial of v
  (define v-fact (locus-channel-put/get ch (ask-factorial id v)))

  (match v-fact
    [(struct answer-factorial (_ _ #false)) (* v (compute-factorial id ch (- v 1)))]
    [(struct answer-factorial (_ _ f)) f]
    [m (error 'compute-factorial "unexpected message, expected answer-factorial, got: ~a" m)]))

(define (print-factorial-table table)
  (for ([i (range 1 (add1 (hash-count table)))])
    (printf "~a ~a~n" i (hash-ref table i))))

(define (main cores* N)
  (define cores (min cores* N))
  (define cache (make-hasheq (list (cons 1 1))))

  (define workers
    (for/hasheq ([i (range cores)])
      (values (igensym) (locus ch (worker-factorial-go ch)))))

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

    (printf "Remaining workers: ~a~n" active-workers)
    (printf "Remaining work: ~a~n" remaining-work)
    (printf "cache: ~a~n" cache)

    (cond
      [(and (null? remaining-work)
            (hash-empty? active-workers))
       (printf "All done~n")
       (print-factorial-table cache)]
      [else
       ; like locus-dead-evt but whose sync value
       ; is the dead locus
       (define evt-dead
         (lambda (p)
           (wrap-evt (locus-dead-evt p)
                     (lambda (v) p))))

       (sync
        (handle-evt
         (apply choice-evt (map dead-evt (hash-values active-workers)))
         (lambda (l)
           (printf "A locus died~n")
           (if (zero? (locus-wait l))
               (loop active-workers remaining-work)
               (error "Locus died unexpectedly with result" (locus-wait l)))))

        (handle-evt
         (apply choice-evt (hash-values active-workers))
         (match-lambda
           [(struct ask-factorial (w v))
            (printf "Worker asking for ~a!~n" v)
            (cond
              [(hash-ref cache v #false)
               => (lambda (f)
                   (printf "master knows ~a! = ~a~n" v f)
                   (locus-channel-put (hash-ref active-workers w)
                                      (answer-factorial 'master v f)))]
              [else
               (printf "master does not know ~a!~n" v)
               (locus-channel-put (hash-ref active-workers w)
                                  (answer-factorial 'master v #false))])
            (loop active-workers remaining-work)]

           [(struct answer-factorial (w v f))
            (printf "Worker giving factorial answer ~a! = ~a~n" v f)
            (hash-set! cache v f)

            (match (locus-channel-get (hash-ref active-workers w))
              [(struct worker-done (wid))
               (when (not (eq? wid w))
                 (error "expected some id from done worker"))
               (define r (locus-wait (hash-ref active-workers w)))
               (printf "Worker ~a is finished with status: ~a~n" w r)]
              [_ (error "unexpected message from worker before finish")])

            (cond
              [(null? remaining-work) (loop (hash-remove active-workers w) '())]
              [else
               (define new-locus (locus ch (worker-factorial-go ch)))
               (define id (igensym))
               (locus-channel-put new-locus (msg-id id))
               (locus-channel-put new-locus (compute-v 'master (car remaining-work)))
               (printf "Requesting new locus ~a for ~a~n" id (car remaining-work))
               (loop (hash-set
                      (hash-remove active-workers w)
                      id new-locus)
                     (rest remaining-work))])]

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
