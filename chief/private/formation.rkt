#lang at-exp racket/base

(require gregor
         racket/format
         racket/match
         racket/set
         racket/system
         "ansi.rkt"
         "procfile.rkt")

(provide
 start-formation)

(define (start-formation #:env env
                         #:defs defs
                         #:procs procs
                         #:formation [formation (hash)])

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

  (define longest-id
    (apply max (map string-length (hash-keys subprocesses))))

  (define (log id ts message)
    (colorize
     (make-process-color id)
     (display (~a (~t ts "HH:mm:ss") " " id
                  #:min-width (+ longest-id 9)))
     (display " | "))
    (displayln message))

  (define stopping? #f)
  (define (stop-all [signal 'interrupt])
    (set! stopping? #t)
    (for ([p (in-hash-values subprocesses)])
      (p signal)))

  (define logger
    (thread
     (lambda _
       (file-stream-buffer-mode (current-output-port) 'line)

       (let loop ()
         (match (sync ch)
           [(list 'exit id ts code)
            (log id ts @~a{process exited with code @(~c code)})
            (set-remove! running-processes id)
            (unless stopping?
              (stop-all))]

           [(list (or 'stdout 'stderr) id ts message)
            (log id ts message)])

         (unless (set-empty? running-processes)
           (loop))))))

  (let loop ()
    (with-handlers ([exn:break?
                     (lambda _
                       (stop-all (if stopping?
                                     'kill
                                     'interrupt))
                       (loop))])
      (sync/enable-break logger)
      (custodian-shutdown-all custodian))))

(define (start-subprocess ch id command)
  (match-define (list stdout stdin pid stderr control)
    (process command))

  (define (emit event . args)
    (channel-put ch (list* event id (now) args)))

  (thread
   (lambda _
     (dynamic-wind
       (lambda _
         (emit 'stdout @~a{process started with pid @pid}))
       (lambda _
         (let loop ()
           (define ((handle-output event) in)
             (define line (read-line in))
             (unless (eof-object? line)
               (emit event line)
               (loop)))

           (sync
            (handle-evt stdout (handle-output 'stdout))
            (handle-evt stderr (handle-output 'stderr)))))
       (lambda _
         (emit 'exit (control 'exit-code))))))

  control)

(define process-colors
  #(red green yellow blue magenta cyan))

(define (make-process-color id)
  (list (list'fg (vector-ref process-colors (modulo (make-string-hash id)
                                                    (vector-length process-colors))))))

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
