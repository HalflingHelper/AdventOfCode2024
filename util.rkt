#lang racket

(require "private.rkt")

(provide get-input
         sum
         all?
         char->num)
;; Stuff for pulling the input more efficiently
;; I'm writing my own library for this instead of using the package :)
(require net/url)
(require racket/format)

(define (make-input-request day)
  (port->string (get-pure-port (string->url (format "https://adventofcode.com/2024/day/~a/input" day))
                               (list (format "Cookie: ~a" PRIV_COOKIE)))))

(define (pad-day day)
  (~a day #:min-width 2 #:align 'right #:left-pad-string "0"))

(define (get-input-path day)
  (format "inputs/input_~a.txt" (pad-day day)))

(define (get-full-input day)
  (let ([fname (get-input-path day)])
    (if (file-exists? fname)
        (file->lines fname)
        (let ([raw (make-input-request day)])
          (begin
            (display-to-file raw fname)
            (string-split raw "\n"))))))

(define (get-input day [test? #f])
  (if test?
      (file->lines "inputs/tmp.txt")
      (get-full-input day)))

;; Utility functions that I maybe want
(define sum ((curry apply) +))

(define (all? ls pred)
  (andmap pred ls))

(define (char->num c) (string->number (make-string 1 c)))
