#lang racket/base

(require racket/function
         racket/match)

(provide
 colorize)

(define (decorate decoration out)
  (case decoration
    [(bold)      (display "\e[1m" out)]
    [(underline) (display "\e[4m" out)]
    [(reversed)  (display "\e[7m" out)]
    [else (raise-argument-error 'decorate "a valid decoration" decoration)]))

(define (fg-color color out)
  (define num
    (case color
      [(black)   "30;1"]
      [(red)     "31;1"]
      [(green)   "32;1"]
      [(yellow)  "33;1"]
      [(blue)    "34;1"]
      [(magenta) "35;1"]
      [(cyan)    "36;1"]
      [(white)   "37;1"]
      [else (raise-argument-error 'fg-color "a valid fg color" color)]))

  (display (string-append "\e[" num "m") out))

(define (bg-color color out)
  (define num
    (case color
      [(black)   "40"]
      [(red)     "41"]
      [(green)   "42"]
      [(yellow)  "43"]
      [(blue)    "44"]
      [(magenta) "45"]
      [(cyan)    "46"]
      [(white)   "47"]
      [else (raise-argument-error 'bg-color "a valid bg color" color)]))

  (display (string-append "\e[" num "m") out))

(define (color spec [out (current-output-port)])
  (for ([pair (in-list spec)])
    (match pair
      [(list 'fg color decorations ...)
       (fg-color color out)
       (for-each (curryr decorate out) decorations)]

      [(list 'bg color)
       (bg-color color out)])))

(define (reset [out (current-output-port)])
  (display #"\e[0m" out))

(define (call-with-colorized-output spec proc)
  (dynamic-wind
    (lambda _ (color spec))
    (lambda _ (proc))
    (lambda _ (reset))))

(define-syntax-rule (colorize spec body0 body ...)
  (call-with-colorized-output spec (lambda _ body0 body ...)))

(module+ test
  (require racket/port
           rackunit)

  (check-equal?
   (with-output-to-string
     (lambda _
       (colorize
        '((fg red))
        (display "hello!"))))
   "\e[31;1mhello!\e[0m")

  (check-equal?
   (with-output-to-string
     (lambda _
       (colorize
        '((fg red)
          (bg white))
        (display "hello!"))))
   "\e[31;1m\e[47mhello!\e[0m")

  (check-equal?
   (with-output-to-string
     (lambda _
       (colorize
        '((fg red bold)
          (bg white))
        (display "hello!"))))
   "\e[31;1m\e[1m\e[47mhello!\e[0m"))
