#lang info

(define version "0.1")
(define collection "chief")

(define deps '("base"
               "gregor-lib"))

(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define test-omit-paths '("cli.rkt"))
(define raco-commands '(("chief" chief/cli "run application processes" #f)))
