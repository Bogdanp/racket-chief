#lang at-exp racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/cmdline
         racket/format
         racket/hash
         racket/match
         racket/string
         racket/system
         raco/command-name
         "private/env.rkt"
         "private/formation.rkt"
         "private/procfile.rkt")

(define current-program-name
  (make-parameter (short-program+command-name)))

(define current-envfile-paths
  (make-parameter (list)))

(define current-procfile-path
  (make-parameter "Procfile"))

(define current-formation
  (make-parameter (hash)))

(define (exit-with-errors! . messages)
  (parameterize ([current-output-port (current-error-port)])
    (for ([message messages])
      (displayln message)))
  (exit 1))

(define (read-envfile path missing-ok?)
  (parameterize ([port-count-lines-enabled #t])
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e)
                       (cond
                         [missing-ok? (hash)]
                         [else (exit-with-errors! @~a{error: failed to open '@path'})]))]

                    [exn:fail?
                     (lambda (e)
                       (exit-with-errors! @~a{error: '@path' is not valid}
                                          @~a{  @(exn-message e)}))])
      (call-with-input-file path read-env))))

(define (read-envfiles)
  (define-values (paths missing-ok?)
    (if (null? (current-envfile-paths))
        (values (list ".env") #t)
        (values (current-envfile-paths) #f)))

  (define vars
    (for/fold ([env (hash)])
              ([path (in-list paths)])
      (hash-union env (read-envfile path missing-ok?) #:combine (lambda (v1 v2) v2))))

  (define env
    (environment-variables-copy (current-environment-variables)))

  (begin0 env
    (for ([(name value) vars])
      (environment-variables-set! env name value))))

(define (read-procfile)
  (parameterize ([port-count-lines-enabled #t])
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e)
                       (exit-with-errors! @~a{error: failed to open '@(current-procfile-path)'}))]

                    [exn:fail?
                     (lambda (e)
                       (exit-with-errors! @~a{error: '@(current-procfile-path)' is not valid}
                                          @~a{  @(exn-message e)}))])
      (call-with-input-file (current-procfile-path) read-procdefs))))

(define formation-re
  #rx"^([^=]+)=(0|[1-9][0-9]*)$")

(define (read-formation spec)
  (match (regexp-match formation-re spec)
    [#f (exit-with-errors! @~a{error: invalid formation '@spec'})]
    [(list _ proc num) (values proc (string->number num))]))

(define-syntax (chief-command-line stx)
  (syntax-parse stx
    [(_ (~alt
         (~optional (~seq #:once-each oe:expr ...+) #:defaults ([(oe 1) null]))
         (~optional (~seq #:multi me:expr ...+)     #:defaults ([(me 1) null]))
         (~optional (~seq #:args ae:expr)           #:defaults ([ae #'args]))) ...
        body:expr ...+)
     #'(command-line
        #:program (current-program-name)
        #:once-each
        [("-f" "--procfile")
         path
         "Specify an alternate Procfile <path> to load (default: Procfile)"
         (current-procfile-path path)]
        oe ...
        #:multi
        [("-e" "--env")
         path
         "Specify one or more env file <path>s to load (default: .env)"
         (current-envfile-paths (cons path (current-envfile-paths)))]
        me ...
        #:args ae
        body ...)]))

(define (handle-check)
  (define-values (env procdefs)
    (chief-command-line
     (values (read-envfiles)
             (read-procfile))))

  (display "valid procfile detected (")
  (display (string-join (map procdef-name procdefs) ", "))
  (displayln ")"))

(define (handle-help)
  (exit-with-errors!
   "usage: raco chief <command> <option> ... <arg> ..."
   ""
   "available commands:"
   "  check    validate a Procfile"
   "  help     print this message and exit"
   "  run      run a command using your application's environment"
   "  start    start the application (or a specific process)"))

(define (handle-run)
  (chief-command-line
   #:args (command . command-args)
   (parameterize ([current-environment-variables (read-envfiles)])
     (exit (system/exit-code (string-join (cons command command-args) " "))))))

(define (handle-start)
  (chief-command-line
   #:multi
   [("-m" "--formation")
    spec
    "Specify how many processes of one type to run. (example: 'web=2')"
    (let-values ([(process num) (read-formation spec)])
      (current-formation (hash-set (current-formation) process num)))]
   #:args processes
   (define env (read-envfiles))
   (define procdefs (read-procfile))
   (define known-processes
     (map procdef-name procdefs))

   (for ([name (in-list processes)])
     (unless (member name known-processes)
       (exit-with-errors! @~a{error: invalid process '@name'})))

   (start-formation #:env env
                    #:defs procdefs
                    #:procs (if (null? processes)
                                known-processes
                                processes)
                    #:formation (current-formation))))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

(define all-commands
  (hasheq 'check handle-check
          'help  handle-help
          'run   handle-run
          'start handle-start))

(define-values (command handler args)
  (match (current-command-line-arguments)
    [(vector command args ...)
     (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]

    [_
     (values "help" handle-help null)]))

(parameterize ([current-command-line-arguments (list->vector args)]
               [current-program-name (~a (current-program-name) " " command)])
  (handler))
