#lang racket/base

(require gregor
         racket/format
         racket/match
         racket/set
         racket/system
         "procfile.rkt"
         "term.rkt")

(provide
 start-formation)

(define (start-formation #:env env
                         #:defs defs
                         #:procs procs
                         #:formation [formation (hash)])

  (file-stream-buffer-mode (current-output-port) 'line)

  (define ch (make-channel))
  (define custodian (make-custodian))
  (define subprocesses
    (parameterize ([current-custodian custodian])
      (for/fold ([subprocesses (hash)])
                ([def (in-list defs)]
                 [port (in-range 5000 65536 100)])
        (define name (procdef-name def))
        (define command (procdef-command def))
        (for/fold ([subprocesses subprocesses])
                  ([n (in-range (hash-ref formation name 1))]
                   #:when (member name procs))

          (define id (format "~a.~a" name (add1 n)))
          (define proc-env (environment-variables-copy env))
          (environment-variables-set! proc-env #"PS" (string->bytes/utf-8 id))
          (environment-variables-set! proc-env #"PORT" (string->bytes/utf-8 (number->string (+ port n))))
          (parameterize ([current-environment-variables proc-env])
            (hash-set subprocesses id (start-subprocess ch id command)))))))

  (define running-processes
    (apply mutable-set (hash-keys subprocesses)))

  (define topic-width
    ;; 2 characters for the :s, 6 for the time digits and one space
    (+ 9 (apply max (map string-length (list* "system" (hash-keys subprocesses))))))

  (define (~topic . args)
    (apply ~a args #:min-width topic-width))

  (define (log message [id 'system] [ts (now)])
    (colorize
     (make-process-color id)
     (display (~topic (~t ts "HH:mm:ss") " " id))
     (display " | "))
    (displayln message))

  (define stopping? #f)
  (define (stop-all [signal 'interrupt])
    (set! stopping? #t)
    (for ([p (in-hash-values subprocesses)])
      (p signal)))

  (define (event-loop)
    (match (sync/enable-break ch)
      [(list 'exit id ts code)
       (log (format "process exited with code ~a" (~c code)) id ts)
       (set-remove! running-processes id)

       (unless stopping?
         (log (format "stopping all processes because '~a' died" id))
         (stop-all))]

      [(list 'message id ts message)
       (log message id ts)])

    (unless (set-empty? running-processes)
      (event-loop)))

  (let loop ()
    (with-handlers ([exn:break?
                     (lambda _
                       (define signal
                         (cond
                           [stopping?
                            (begin0 'kill
                              (log "killing all processes"))]

                           [else
                            (begin0 'interrupt
                              (log "stopping all processes"))]))

                       (stop-all signal)
                       (loop))])
      (event-loop))))

(define (start-subprocess ch id command)
  (match-define (list stdout stdin pid stderr control)
    (process command))

  (define (emit event . args)
    (channel-put ch (list* event id (now) args)))

  (thread
   (lambda _
     (dynamic-wind
       (lambda _
         (emit 'message (format "process started with pid ~a" pid)))
       (lambda _
         (let loop ()
           (define line (read-line (sync (choice-evt stdout stderr))))
           (unless (eof-object? line)
             (emit 'message line)
             (loop))))
       (lambda _
         (emit 'exit (control 'exit-code))))))

  control)

(define process-colors
  (for*/vector ([r (in-range 1 6)]
                [g (in-range 1 6)]
                [b (in-range 1 6)])
    (make-color r g b)))

(define (make-process-color id)
  (if (eq? id 'system)
      `((fg ,(make-gray 0.75)))
      `((fg ,(vector-ref process-colors
                         (modulo (make-string-hash id)
                                 (vector-length process-colors)))))))

(define (make-string-hash s)
  (define basis 2166136261)
  (define prime 16777619)
  (define scale (expt 2 32))

  (for/fold ([h basis])
            ([b (in-bytes (string->bytes/utf-8 s))])
    (((bitwise-xor h b) . * . prime) . modulo . scale)))

(define (~c code)
  (if (zero? code)
      0
      (- code 128)))
