#lang info

(define collection 'multi)

(define pkg-desc "Racket parallel code execution as separate OS processes")
(define version "0.1")
(define pkg-authors '(pmatos))

(define deps '("base" "unix-socket-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define compile-omit-paths '("tests"))
