#lang racket/base

(require racket/match)

(provide
 read-env)

(define vardef-re
  #rx"(?m:([^=]+)=(.*))\n?")

(define (read-env-var in)
  (define-values (line col pos)
    (port-next-location in))

  (match (regexp-match vardef-re in)
    [#f
     (if (and line col)
         (error 'read-env-var "invalid variable definition at ~a:~a" line col)
         (error 'read-env-var "invalid variable definition at pos ~a" pos))]

    [(list _ name value)
     (values name value)]))

(define (read-env [in (current-input-port)])
  (let loop ([vars (hash)])
    (match (peek-char in)
      [(? eof-object?) vars]

      [#\#
       (read-line in)
       (loop vars)]

      [_
       (define-values (name value)
         (read-env-var in))

       (loop (hash-set vars name value))])))

(module+ test
  (require rackunit)

  (check-equal?
   (read-env (open-input-string "A=42"))
   (hash #"A" #"42"))

  (check-equal?
   (read-env (open-input-string #<<ENV
A=42
B=20
# a comment
A=1
ENV
                                ))
   (hash #"A" #"1"
         #"B" #"20")))
