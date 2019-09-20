#lang racket/base

(require racket/match)

(provide
 (struct-out procdef)
 read-procdef
 read-procdefs)

(define procdef-re
  #rx"(?m:([^:]+):[ ]+(.+))\n?")

(struct procdef (name command)
  #:transparent)

(define (read-procdef [in (current-input-port)])
  (define-values (line col pos)
    (port-next-location in))

  (match (regexp-match procdef-re in)
    [#f
     (if (and line col)
         (error 'read-procdef "invalid process definition at ~a:~a" line col)
         (error 'read-procdef "invalid process definition at pos ~a" pos))]
    [(list _ name command) (procdef (bytes->string/utf-8 name)
                                    (bytes->string/utf-8 command))]))

(define (read-procdefs [in (current-input-port)])
  (let loop ([procdefs null])
    (if (eof-object? (peek-byte in))
        (reverse procdefs)
        (loop (cons (read-procdef in) procdefs)))))

(module+ test
  (require rackunit)

  (check-exn
   exn:fail?
   (lambda _
     (read-procdef (open-input-string ""))))

  (check-exn
   exn:fail?
   (lambda _
     (read-procdef (open-input-string "web:"))))

  (check-equal?
   (read-procdef (open-input-string "web: npm start"))
   (procdef "web" "npm start"))

  (define multiline #<<PROCFILE
assets: npm start
web: raco koyo serve
PROCFILE
    )

  (let ([multiline-in (open-input-string multiline)])
    (check-equal? (read-procdef multiline-in) (procdef "assets" "npm start"))
    (check-equal? (read-procdef multiline-in) (procdef "web" "raco koyo serve")))

  (let ([multiline-in (open-input-string multiline)])
    (check-equal? (read-procdefs multiline-in)
                  (list (procdef "assets" "npm start")
                        (procdef "web" "raco koyo serve")))))
